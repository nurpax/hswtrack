#/usr/bin/python3

# Very brittle little script to import Fitocracy data from JSON into hswtrack
# database.
#
# Fitocracy data can be exported using the tool from
#
# https://github.com/luketurner/fitocracy-export.git
#
# Run the export tool like:
#
# python3 cli.py <username> <pass>
#
# Then feed into your database like so:
#
# python3 fitoimport.py fitocracy_data.json

import json
import sqlite3
import sys
from collections import defaultdict
from itertools import groupby

hswtrackExercises = { 1:  ("Chin-ups", "BW"),
                      2:  ("Barbell Good Morning", "W"),
                      3:  ("Front Barbell Squat", "W"),
                      4:  ("Barbell Bench Press", "W"),
                      5:  ("Dips", "BW"),
                      6:  ("Barbell Deadlift", "W"),
                      7:  ("Hyperextension", "BW"),
                      8:  ("Pull-ups", "BW"),
                      9:  ("Power Clean", "W"),
                      10: ("Barbell Overhead Press (OHP)", "W"),
                      11: ("Barbell Squat", "W"),
                      12: ("Barbell Incline Bench Press", "W"),
                      13: ("Sit-ups", "BW"),
                      14: ("Dumbbell Lunges", "W"),
                      15: ("Leg raises", "BW"),
                      16: ("Push-ups", "BW"),
                      17: ("Push-ups (on knees)", "BW"),
                      18: ("Crunch", "BW"),
                      19: ("Cable Rope Overhead Triceps Extension", "W"),
                      20: ("Upright Barbell Row", "W"),
                      21: ("Incline Dumbbell Bench Press", "W"),
                      22: ("Dumbbell Bicep Curl", "W")
                    }

class Sets():
    def __init__(self, fId, weight, reps):
        self.fitoId = fId
        self.weight = weight
        self.reps   = reps

def fitoExerciseMap():
    d = {}
    d[1]   = 4   # bench
    d[2]   = 11  # squat
    d[3]   = 6   # deadlift
    d[11]  = 18  # crunch
    d[42]  = 22  # dumbbell bicep curl
    d[84]  = 12  # incline bench press
    d[91]  = 5   # dips (chest version)
    d[150] = 7   # hyperextension
    d[151] = 2   # good morning
    d[171] = 14  # dumbbell lunges
    d[174] = 3   # front squat
    d[183] = 10  # ohp
    d[230] = 20  # upright barbell row
    d[245] = 19  # Cable Rope Overhead Triceps Extension
    d[251] = 5   # dips
    d[283] = 1   # chin-ups
    d[288] = 8   # pull-ups
    d[303] = 21  # Incline Dumbbell Bench Press
    d[349] = 13  # sit-ups
    d[472] = 9   # power clean
    return d

fitoExercises = fitoExerciseMap()

def saveToDatabase(setsByDate, userId):
    conn = sqlite3.connect("hswtrack.db")
    existingExercises = [e for (e,) in conn.execute("SELECT id FROM exercises")]

    # Insert exercises NOT already in the database
    for (hsId,(name,bw)) in hswtrackExercises.items():
        if hsId not in existingExercises:
            conn.execute("INSERT INTO exercises (id,name,type) VALUES (?,?,?)", (hsId, name, bw))
            print ("Insert new exercise: " + str(hsId))
    dates = sorted(setsByDate.keys())
    for day in dates:
        daySets = setsByDate[day]
        x = groupby(daySets, lambda s: s.fitoId)
        # Note: we lose the original actiontime here, we just set the
        # time to midday.  This is all sorts of wrong timezone-wise,
        # but works fine for European timezones.
        datetime = day + " 12:00:00"
        # Note: could insert comments too here, schema supports it
        conn.execute("INSERT INTO workouts (timestamp, user_id) VALUES (?,?)", (datetime, userId))
        curs = conn.execute("SELECT last_insert_rowid()")
        workoutId = curs.fetchone()[0]
        # Now insert sets for the workout's exercises
        for (fitoId, sets) in x:
            hsId = fitoExercises[fitoId]
            (_,ty) = hswtrackExercises[hsId]
            for s in sets:
                weight = s.weight
                if ty == 'B':
                    if weight is None:
                        print ("**** s.weight cannot be null for weighted exercises")
                        sys.exit(1)
                if ty == "BW":
                    if weight is None:
                        weight = 0.0
                # Note: could insert comments too here, schema supports it
                conn.execute("INSERT INTO sets (timestamp, user_id, workout_id, exercise_id, reps, weight) VALUES (?,?,?,?,?,?)",
                             (datetime, userId, workoutId, hsId, s.reps, weight))
    conn.commit()
    conn.close()

def __main__(argv):
    if len(argv) < 2:
        print ("Usage: "+argv[0]+" <jsoninput>")
        sys.exit(1)

    doc = json.load(open(argv[1]))
    setsByDate = defaultdict(list)

    dates = {}

    for t in doc: # maybe 't' is pages?
        x = doc[t]
        for x in doc[t]:
            actions = x["actions"]
            for action in actions:
                date = action["actiondate"]
                typeId = action["action"]["id"]
                effort = action["effort0_metric"]
                reps = action["effort1_metric"]
                dates[date] = 1

                # Skip some exercises like:
                #
                # 5   = machine ab crunch
                # 34  = barbell curl
                # 164 = one-arm dumbell row
                # 198 = front db raise
                # 256 = Lying Close-Grip Barbell Triceps Extension Behind The Head
                # 286 = lat pulldown
                # 315 = rowing machine
                # 320 = swimming
                # 518 = running
                # 525 = walking
                # 914 = kettlebell lunge
                # 935 = machine back extension
                # 957 = kettlebell lunge
                if typeId in [5, 34, 164, 198, 256, 286, 315, 320, 518, 525, 914, 935, 957]:
                    print ("Skipping exercise {%d} on {%s}" % (typeId, date))
                    continue

                if not (typeId in fitoExercises):
                    print ("Unknown exercise: "+str(typeId))
                    print ("  name: "+action["action"]["name"])
                    sys.exit(0)

                (name,ty) = hswtrackExercises[fitoExercises[typeId]]
                # Fitocracy bodyweight exercises like chin-ups &
                # pull-ups (at least) store the extra weight in the
                # effort3_metric field, not in effort0_metric.  So
                # override that.
                if ty == 'BW':
                    if action["effort3_metric"] is not None:
                        effort = action["effort3_metric"]

                sets = Sets(typeId, effort, reps)
                setsByDate[date].append(sets)


    dd = sorted(dates.keys())
    print (dd)

    dates = sorted(setsByDate.keys())
    for day in dates:
        print ("DATE: "+day)
        daySets = setsByDate[day]
        x = groupby(daySets, lambda s: s.fitoId)
        for (fitoId, sets) in x:
            hsId = fitoExercises[fitoId]
            (name,ty) = hswtrackExercises[hsId]
            print ("exercise "+name)
            for s in sets:
                print ("  "+str(s.reps) + " x "+str(s.weight) + " kg ("+ty+")")
        print ("")
    # TODO fix userId here
    saveToDatabase(setsByDate, 5)

if __name__ == "__main__":
    __main__(sys.argv)
