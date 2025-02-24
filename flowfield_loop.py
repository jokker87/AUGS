import struct

import pygame
import math
import random


screenStartXAmiga = 0x40
screenStartYAmiga = 0x2C

# Pygame Setup
pygame.init()
WIDTH, HEIGHT = 320, 100
FPS = 50
screen = pygame.display.set_mode((WIDTH, HEIGHT))
clock = pygame.time.Clock()

# Tunnel‑Effekt‑Einstellungen:
NUM_STARS = 100
LOOP_FRAMES = 100  # Loop-Dauer in Frames (t in [0,1))
center_x = WIDTH // 2
center_y = HEIGHT // 2

# Parameter der 3D-Szene:
z_min = 0.5   # Bei local_t = 0: Stern ist "nah" (hell)
z_max = 20.0  # Bei local_t = 1: Stern ist "fern" (dunkler)
focal = 500   # Fokalabstand für die Perspektivprojektion

# Initialisiere die Sterne.
# Jeder Stern bekommt:
# - einen zufälligen Winkel (theta) für seine Position auf der Zylinderfläche
# - einen radialen Abstand (R) vom Zylinderzentrum, der nun so gewählt wird, dass
#   der Mindestwert höher liegt (hier zwischen ca. 1.5 und 3.0), sodass weniger Sterne
#   direkt in der Mitte starten.
# - einen zufälligen Phasenoffset (phase) in [0,1) für den Startpunkt im Loop
# - einen Speed-Faktor: ca. 70% der Sterne bewegen sich mit Speed 1, 30% mit Speed 2
stars = []
for _ in range(NUM_STARS):
    theta = random.uniform(0, 2 * math.pi)
    # Statt früherer Verteilungen wählen wir R nun gleichmäßig aus dem Bereich [1.5, 3.0]:
    R = 2 * (0.5 + 0.5 * random.random())
    phase = random.uniform(0, 1)
    # 70% der Sterne mit Speed 1, 30% mit Speed 2:
    speed = 1 if random.random() < 0.7 else 2
    stars.append({"theta": theta, "R": R, "phase": phase, "speed": speed})

def distribute_pixels_evenly(pixels, num_buckets=8):
    # Extrahiere die x- und y-Werte als eine Liste von Tupeln und sortiere nach y
    pixels_sorted = sorted([(entry[1], entry[2]) for entry in pixels], key=lambda p: p[1])

    # Erstelle leere Buckets und eine Liste für entfernte Pixel
    buckets = [[] for _ in range(num_buckets)]
    removed_pixels = []
    last_y_in_bucket = [-1] * num_buckets  # Speichert das letzte y pro Bucket

    bucket_order = list(range(num_buckets))  # Liste der Bucket-Indices für gleichmäßige Verteilung
    bucket_index = 0  # Startindex für Buckets

    for index, (x, y) in enumerate(pixels_sorted):
        if x < 0 or x > WIDTH or y < 0 or y > HEIGHT:
            continue
        assigned = False

        # Probiere, das Pixel dem nächsten Bucket zuzuweisen
        for _ in range(num_buckets):
            current_bucket = bucket_order[bucket_index]  # Hole den aktuellen Bucket
            if last_y_in_bucket[current_bucket] == -1 or (y - last_y_in_bucket[current_bucket] > 1):
                buckets[current_bucket].append((index, x, y))  # Speichere den ursprünglichen Index
                last_y_in_bucket[current_bucket] = y
                assigned = True
                break

            # Falls der Bucket nicht geht, probiere den nächsten in der Reihenfolge
            bucket_index = (bucket_index + 1) % num_buckets

        # Falls das Pixel nicht untergebracht werden kann, entferne es
        if not assigned:
            removed_pixels.append((index, x, y))

        # Wechsel zum nächsten Bucket für die nächste Runde, um die Verteilung auszugleichen
        bucket_index = (bucket_index + 1) % num_buckets

    return buckets, removed_pixels

frame_count = 0
running = True

frames = list()

while running:
    # Normierter globaler Zeitparameter t in [0,1)
    t = (frame_count % LOOP_FRAMES) / LOOP_FRAMES
    screen.fill((0, 0, 0))

    starposition = list()
    for index,star in enumerate(stars):
        # Berechne für jeden Stern einen lokalen, periodischen Zeitwert.
        # Der Speed-Faktor sorgt dafür, dass manche Sterne schneller durch den Tunnel fließen.
        local_t = (star["phase"] - t * star["speed"]) % 1

        # Bestimme den Tiefenwert z:
        z = z_min + local_t * (z_max - z_min)

        # 3D-Position auf der Zylinderfläche (x3d, y3d bleiben konstant, nur z ändert sich)
        x3d = star["R"] * math.cos(star["theta"])
        y3d = star["R"] * math.sin(star["theta"])

        # Perspektivische Projektion
        screen_x = center_x + (x3d / z) * focal
        screen_y = center_y + (y3d / z) * focal

        # Konstante Größe (hier 1 Pixel)
        size = 1

        # Berechne den Grauton: Je näher (kleiner local_t) desto heller.
        brightness = int(50 + 205 * (1 - local_t))
        brightness = 125
        color = (brightness, brightness, brightness)

        pygame.draw.circle(screen, color, (int(screen_x), int(screen_y)), size)
        starposition.append((index,int(screen_x),int(screen_y)))

    frames.append(starposition)
    if len(frames) == LOOP_FRAMES:
        break
    pygame.display.flip()
    clock.tick(FPS)
    frame_count += 1

    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False

countErrorsTotal = 0
for frame in frames:
    countErrors = 0
    #for index,star in enumerate(frame):
        #print(str(index)+": ",end='')
        #print(int(star["pos"][0]),end='')
        #print(", ",end='')
        #print(int(star["pos"][1]))

    # Buckets generieren und problematische Pixel entfernen
    buckets, removed_pixels = distribute_pixels_evenly(frame)



    # Ergebnisse ausgeben
    for i, bucket in enumerate(buckets):
        print(f"Bucket {i + 1} ({len(bucket)} Pixel): ",end='')
        for star in bucket:
            print("(", end='')
            print(int(star[1]), end='')
            print(",", end='')
            print(int(star[2]),end='')
            print(")",end='')
        print()

    print("\nEntfernte Pixel (wegen Regelverstoß):")
    if len(removed_pixels) > 0:
        print("!!!!!!!!!!!!")
        countErrors += 1
    for pixel in removed_pixels:
        print(pixel)

    print("Errors: "+str(countErrors))
    print()
    print()

    countErrorsTotal += countErrors

print("Errors total:"+str(countErrorsTotal))

if countErrorsTotal == 0:
    # Konstanten
    MAGIC_WORD = 0xDEAD  # Magic word zum Frameende
    WORD_SIZE = 2  # 1 Word = 2 Byte
    HEADER_WORDS = 9  # 8 Offsets für Buckets + 1 für nächsten Frame
    header_size = HEADER_WORDS * WORD_SIZE
    # Schreibe die Daten in eine Binärdatei
    with open("frame.bin", "wb") as f:

        for frame in frames:


            # Berechne die Offsets der einzelnen Buckets
            offsets = []
            current_offset = header_size  # Bucket-Daten starten direkt nach dem Header

            buckets, removed_pixels = distribute_pixels_evenly(frame)

            for bucket in buckets:
                offsets.append(current_offset)
                # Für jeden Bucket: 4 Byte für die Pixelanzahl + 4 Byte pro Pixel (2 Byte für x und 2 Byte für y)
                bucket_size = len(bucket) * 8 + 4
                current_offset += bucket_size

            # Offset zum nächsten Frame (falls nicht vorhanden, z.B. 0)
            next_frame_offset = current_offset


            # Header schreiben: 8 Offsets für die Buckets
            for off in offsets:
                f.write(struct.pack(">H", off))
            # Word für das Offset zum nächsten Frame
            f.write(struct.pack(">H", next_frame_offset))

            # Schreibe alle Bucket-Daten hintereinander
            for bucket in buckets:
                if len(bucket) == 0:
                    test = ""
                # Schreibe die Anzahl der Pixel (unsigned int)
                # f.write(struct.pack("I", len(bucket)))
                # Schreibe für jedes Pixel die x- und y-Koordinate (unsigned short jeweils)
                for (index, x, y) in bucket:
                    xAmiga = x + screenStartXAmiga
                    highbitX = 0
                    if xAmiga > 255:
                        highbitX = 1
                        xAmiga = xAmiga - 0xff
                    yAmigaStart = y + screenStartXAmiga
                    yAmigaEnd = y + screenStartXAmiga + 1
                    try:
                        f.write(struct.pack(">B", yAmigaStart))
                    except:
                        print("YStart:")
                        print(yAmigaStart)
                        pass
                    f.write(struct.pack(">B", xAmiga))

                    try:
                        f.write(struct.pack(">B", yAmigaEnd))
                    except:
                        print("YEnd:")
                        print(yAmigaEnd)
                        pass


                    f.write(struct.pack(">B", highbitX))

                    f.write(struct.pack(">B", 0))
                    f.write(struct.pack(">B", 1))
                    f.write(struct.pack(">B", 0))
                    f.write(struct.pack(">B", 0))
                f.write(struct.pack(">L", 0))

        # Am Ende das Magic Word schreiben
        f.write(struct.pack(">H", MAGIC_WORD))



else:
    print("Errors found")
pygame.quit()
