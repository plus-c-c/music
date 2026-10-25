#!/usr/bin/env python3
"""Run one Aria-AMT transcription on CPU.

This helper is copied to the remote Aria-AMT checkout by transcribe.sh.  It
requires the CPU patch in aria-amt-cpu.patch and a checkpoint at
checkpoints/piano-medium-double-1.0.safetensors.
"""

import argparse
import logging
import os
import time

import torch

from amt.audio import AudioTransform
from amt.config import load_model_config
from amt.inference.model import AmtEncoderDecoder, ModelConfig
from amt.inference.transcribe import process_segments, transcribe_file
from amt.tokenizer import AmtTokenizer
from amt.utils import _load_weight


class SyncQueue:
    """A tiny synchronous stand-in for Aria-AMT's GPU queue."""

    def __init__(self, model, audio_transform, tokenizer, logger, results):
        self.model = model
        self.audio_transform = audio_transform
        self.tokenizer = tokenizer
        self.logger = logger
        self.results = results

    def put(self, item, block=True, timeout=None):
        (audio_seg, prefix), pid = item
        started = time.time()
        result = process_segments(
            [((audio_seg, prefix), pid)],
            self.model,
            self.audio_transform,
            self.tokenizer,
            self.logger,
        )
        self.results[pid] = result[0]
        print(
            f"  segment {pid} done in {time.time() - started:.1f}s",
            flush=True,
        )


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--audio", required=True)
    parser.add_argument("--out", required=True)
    parser.add_argument(
        "--checkpoint",
        default="checkpoints/piano-medium-double-1.0.safetensors",
    )
    parser.add_argument("--model", default="medium-double")
    parser.add_argument("--threads", type=int, default=3)
    parser.add_argument("--start", type=float, default=None)
    parser.add_argument("--end", type=float, default=None)
    args = parser.parse_args()

    torch.set_num_threads(max(1, args.threads))
    logger = logging.getLogger("aria-cpu")
    logger.setLevel(logging.INFO)
    handler = logging.StreamHandler()
    handler.setLevel(logging.INFO)
    logger.addHandler(handler)

    tokenizer = AmtTokenizer()
    config = ModelConfig(**load_model_config(args.model))
    config.set_vocab_size(tokenizer.vocab_size)
    model = AmtEncoderDecoder(config)
    state = _load_weight(ckpt_path=args.checkpoint)
    state = {
        key[len("_orig_mod.") :] if key.startswith("_orig_mod.") else key: value
        for key, value in state.items()
    }
    model.load_state_dict(state)
    model.eval()
    model.to("cpu")
    model.decoder.setup_cache(
        batch_size=1,
        max_seq_len=4096,
        max_audio_len=1500,
        dtype=torch.float,
    )

    audio_transform = AudioTransform()
    results = {}
    queue = SyncQueue(model, audio_transform, tokenizer, logger, results)
    segment = (args.start, args.end)
    if args.start is None or args.end is None:
        segment = None

    started = time.time()
    sequence = transcribe_file(
        args.audio,
        queue,
        results,
        pid=0,
        tokenizer=tokenizer,
        segment=segment,
    )
    print(
        f"decoded tokens in {time.time() - started:.1f}s, len={len(sequence)}",
        flush=True,
    )

    last_onset = 0
    for token in reversed(sequence):
        if isinstance(token, tuple) and token[0] == "onset":
            last_onset = token[1]
            break
    midi_dict = tokenizer.detokenize(
        tokenized_seq=sequence,
        len_ms=last_onset,
    )
    midi_dict.remove_redundant_pedals()
    midi = midi_dict.to_midi()
    os.makedirs(os.path.dirname(args.out) or ".", exist_ok=True)
    midi.save(args.out)
    print(f"wrote {args.out}", flush=True)


if __name__ == "__main__":
    main()
