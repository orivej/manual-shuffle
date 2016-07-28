#!/usr/bin/env python
import random

from midi import *

pitches = [C_4, G_4, D_5, A_5, E_6]
tune_seq = [2, 1, 0, 1, 2, 3, 4, 3, 2]
unit = 2
resolution = 2
n = 200
out_path = 'shuffle.mid'


def chord(pitch, duration=2):
    return [
        NoteOnEvent(velocity=80, pitch=pitch),
        NoteOnEvent(velocity=60, pitch=pitch+3),
        NoteOnEvent(velocity=60, pitch=pitch+7),
        NoteOffEvent(tick=duration*unit, pitch=pitch),
        NoteOffEvent(pitch=pitch+3),
        NoteOffEvent(pitch=pitch+7),
    ]


def tune():
    return sum([chord(pitches[i], 1) for i in tune_seq], [])


def main():
    track = Track()
    track += [NoteOffEvent(tick=unit)]
    track += tune()
    for _ in xrange(n):
        track += chord(random.choice(pitches))
    track += [EndOfTrackEvent(tick=unit)]

    pattern = Pattern(resolution=resolution, tracks=[track])
    write_midifile(out_path, pattern)


if __name__ == '__main__':
    main()
