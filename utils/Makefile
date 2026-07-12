CXX = g++
CXXFLAGS = -std=c++11 -fPIC -O2 -Wall -I../modules
LDFLAGS_LINUX = -lasound -lpthread
LDFLAGS_MACOS = -framework CoreMIDI -framework CoreAudio -framework CoreFoundation
LDFLAGS_WINDOWS = -lwinmm

# Source files
MODULE_DIR = ../modules
SRC = $(MODULE_DIR)/rtmidi_c.cpp $(MODULE_DIR)/RtMidi.cpp
OBJ = rtmidi_c.o RtMidi.o

# Target
TARGET = librtmidi.so

.PHONY: all clean linux macosx

all: linux

linux: CXXFLAGS += -D__LINUX_ALSA__
linux: LDFLAGS = $(LDFLAGS_LINUX)
linux: $(TARGET)

macosx: CXXFLAGS += -D__MACOSX_CORE__
macosx: LDFLAGS = $(LDFLAGS_MACOS)
macosx: $(TARGET)

$(TARGET): $(OBJ)
	$(CXX) -shared -o $@ $^ $(LDFLAGS)

%.o: $(MODULE_DIR)/%.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

rtmidi_c.o: $(MODULE_DIR)/rtmidi_c.cpp $(MODULE_DIR)/rtmidi_c.h $(MODULE_DIR)/RtMidi.h
	$(CXX) $(CXXFLAGS) -c -o $@ $<

RtMidi.o: $(MODULE_DIR)/RtMidi.cpp $(MODULE_DIR)/RtMidi.h
	$(CXX) $(CXXFLAGS) -c -o $@ $<

clean:
	rm -f $(OBJ) $(TARGET)
