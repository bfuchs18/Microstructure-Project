# The purpose of this script is to calculate the following variables for the microstrucutre project:

# InterBiteInterval (IBI) = interval between 2 bites (eat_f and eat_f) in seconds
# InterEatingInterval_f (InterEatInt_f) = interval between end of 1 bite (eat_f) and start of next bite (eat_f) in seconds
# InterEatingInterval_n (InterEatInt_n)= interval between end of chewing (eat_n) and start of next bite (eat_f) in seconds
## note, if >1 eat_n follow a eat_f, the end of the last eat_n will be considered the end of the episode

# Script written by Bari Fuchs (with help from J.VonFricken) on July 25 2020.

#Make a list of the text files in 
import os
import csv
import datetime
import pprint

path = "/Users/barifuchs/Box/Bari_files/00_PennState/Bout_Project/Data/Annotation/"
#path = "/Users/baf44/Box/Bari_files/00_PennState/Bout_Project/Data/Annotation/"
filenames = os.listdir(path)


#for filename in filenames: # for every file in the directory
for filename in ['AUR0038_visit3.csv']: #has missing data (question mark) in timecolumn
#for filename in ['AUR0029_visit1.csv']: #file with no missing data or drinks
    prefix = filename.split('.')[0] # get prefix of file
    print(prefix)
    if 'AUR' in filename: 
        data = [] # create empty list... this will become a list of dictionaries
        with open (path+filename) as file:
            csv_reader = csv.reader(file)
            next(csv_reader) #skip the first row (header information)
            i = 0
            for line in csv_reader: #for every line in csv
                sampledict = {} # create a dictionary
                # create keys (i.e. "index", "start_time") for each dictionary ... each key will have 1 value
                sampledict["id"] = i
                sampledict["start_time"] = line[0].strip("[]") #starttime is value in 1st column
                sampledict["end_time"] = line[1].strip("[]") #Endtime is value in 2st column
                sampledict["Annotation"] = line[2] #Endtime is value in 3rd column
                sampledict["chewtimes"] = line[3]
                sampledict["category"] = line[4]
                sampledict["Foodtype"] = line[5]
                # add dictionary to the list 'data'
                data.append(sampledict)
                i = i + 1
            #print(data)[0]

    # Separate data from meal and snack paradigms into separate lists
            data_meal = [] #create list for meal data
            i = 0
            for sampledict in data:
                if sampledict["Annotation"] != "three taps end" :
                    data_meal.append(sampledict)
                if sampledict["Annotation"] == "three taps end" :
                    break

                i = i+1
                #print data_meal
                #print(len(data_meal))
                
            data_snack = data[i+1::]

    # make list of dictionaries for each NEW BITE (annotation eat_f) in meal paradigm
        data_meal_fonly = []
        for sampledict in data_meal:
            if sampledict["Annotation"] == "eat_f" :
                data_meal_fonly.append(sampledict)

    # make list of dictionaries for NEW BITE in snack paradigm
        data_snack_fonly = []
        for sampledict in data_snack:
            if sampledict["Annotation"] == "eat_f" :
                data_snack_fonly.append(sampledict)

    # make list of dictionaries for each eating occasion (annotations eat_f and eat_n) in meal paradigm
        data_meal_fn = []
        for sampledict in data_meal:
            if sampledict["Annotation"] == "eat_f" or sampledict["Annotation"] == "eat_n":
                data_meal_fn.append(sampledict)

    # make list of dictionaries for each eating occasion (annotations eat_f and eat_n) in snack paradigm
        data_snack_fn = []
        for sampledict in data_snack:
            if sampledict["Annotation"] == "eat_f" or sampledict["Annotation"] == "eat_n":
                data_snack_fn.append(sampledict)

    #### Dealing with '?' in time column:
    # If prev_end_time == ?, cant calculate IBI
    # If start_time == ?, cant calculate IBI
    
    
    # Calculate IBI and InterEatingInterval_f for Meal
        i = 0
        for i in range(len(data_meal_fonly)):
            if i == 0:
                continue
            
            prev_start_time_str = data_meal_fonly[i-1]["start_time"]
            prev_end_time_str = data_meal_fonly[i-1]["end_time"]
            start_time_str = data_meal_fonly[i]["start_time"]

            if prev_start_time_str == '?' or start_time_str == "?":
                data_meal_fonly[i]["IBI"] = 'NA'

            if prev_end_time_str == '?' or start_time_str == "?":
                data_meal_fonly[i]["InterEatInt_f"] = 'NA'

            if prev_start_time_str != '?' and start_time_str != "?":
                prev_start_time_obj = datetime.datetime.strptime(prev_start_time_str, '%H:%M:%S')
                start_time_obj = datetime.datetime.strptime(start_time_str, '%H:%M:%S')
                data_meal_fonly[i]["IBI"] = (start_time_obj - prev_start_time_obj).total_seconds()

            if prev_end_time_str != '?' and start_time_str != "?":
                start_time_obj = datetime.datetime.strptime(start_time_str, '%H:%M:%S')
                prev_end_time_obj = datetime.datetime.strptime(prev_end_time_str, '%H:%M:%S')
                data_meal_fonly[i]["InterEatInt_f"] = (start_time_obj - prev_end_time_obj).total_seconds() #need to fill in with 0 if 0??

            #print(data_meal_fonly[i])
            i = i + 1
        #print(data_meal_fonly[0])
        for dmf in data_meal_fonly:
            if "IBI" in dmf:
                data[dmf["id"]]["IBI"] = dmf["IBI"]
            if "InterEatInt_f" in dmf:
                data[dmf["id"]]["InterEatInt_f"] = dmf["InterEatInt_f"]

    # Calculate IBI and InterEatingInterval_f for Snack
        i = 0
        for i in range(len(data_snack_fonly)):
            if i == 0:
                continue
            prev_start_time_str = data_snack_fonly[i-1]["start_time"]
            prev_end_time_str = data_snack_fonly[i-1]["end_time"]
            start_time_str = data_snack_fonly[i]["start_time"]

            if prev_start_time_str == '?' or start_time_str == "?":
                data_snack_fonly[i]["IBI"] = 'NA'

            if prev_end_time_str == '?' or start_time_str == "?":
                data_snack_fonly[i]["InterEatInt_f"] = 'NA'

            if prev_start_time_str != '?' and start_time_str != "?":
                prev_start_time_obj = datetime.datetime.strptime(prev_start_time_str, '%H:%M:%S')
                start_time_obj = datetime.datetime.strptime(start_time_str, '%H:%M:%S')
                data_snack_fonly[i]["IBI"] = (start_time_obj - prev_start_time_obj).total_seconds()

            if prev_end_time_str != '?' and start_time_str != "?":
                start_time_obj = datetime.datetime.strptime(start_time_str, '%H:%M:%S')
                prev_end_time_obj = datetime.datetime.strptime(prev_end_time_str, '%H:%M:%S')
                data_snack_fonly[i]["InterEatInt_f"] = (start_time_obj - prev_end_time_obj).total_seconds() #need to fill in with 0 if 0??

           #print(data_snack_fonly[i])
            i = i + 1

        for dsf in data_snack_fonly:
            if "IBI" in dsf:
                data[dsf["id"]]["IBI"] = dsf["IBI"]
            if "InterEatInt_f" in dsf:
                data[dsf["id"]]["InterEatInt_f"] = dsf["InterEatInt_f"]

    # Calculate InterEatingInterval_n for Meal
        i = 0
        for i in range(len(data_meal_fn)):
            data_meal_fn[i]["Paradigm"] = "Meal"

            if i == 0:
                continue
            prev_start_time_str = data_meal_fn[i-1]["start_time"]
            prev_start_time_obj = datetime.datetime.strptime(prev_start_time_str, '%H:%M:%S')

            prev_end_time_str = data_meal_fn[i-1]["end_time"]
            prev_end_time_obj = datetime.datetime.strptime(prev_end_time_str, '%H:%M:%S')

            start_time_str = data_meal_fn[i]["start_time"]
            start_time_obj = datetime.datetime.strptime(start_time_str, '%H:%M:%S')

            if data_meal_fn[i]["Annotation"] == "eat_f" :
                data_meal_fn[i]["InterEatInt_n"] = (start_time_obj - prev_end_time_obj).total_seconds() #need to fill in with 0 if 0??
           hh 
            if data_meal_fn[i]["Annotation"] == "eat_n" :
                data_meal_fn[i]["InterEatInt_n"] = "NA"
                
        #print(data_meal_fn)[29:35]

        for dmfn in data_meal_fn:
            if "InterEatInt_n" in dmfn:
                data[dmfn["id"]]["InterEatInt_n"] = dmfn["InterEatInt_n"]
            if "Paradigm" in dmfn:
                data[dmfn["id"]]["Paradigm"] = dmfn["Paradigm"]

    # Calculate InterEatingInterval_n for snack
        i = 0
        for i in range(len(data_snack_fn)):
            data_snack_fn[i]["Paradigm"] = "Snack"
            if i == 0:
                continue
            prev_start_time_str = data_snack_fn[i-1]["start_time"]
            prev_start_time_obj = datetime.datetime.strptime(prev_start_time_str, '%H:%M:%S')

            prev_end_time_str = data_snack_fn[i-1]["end_time"]
            prev_end_time_obj = datetime.datetime.strptime(prev_end_time_str, '%H:%M:%S')

            start_time_str = data_snack_fn[i]["start_time"]
            start_time_obj = datetime.datetime.strptime(start_time_str, '%H:%M:%S')

            if data_snack_fn[i]["Annotation"] == "eat_f" :
                data_snack_fn[i]["InterEatInt_n"] = (start_time_obj - prev_end_time_obj).total_seconds() #need to fill in with 0 if 0??
            
            if data_snack_fn[i]["Annotation"] == "eat_n" :
                data_snack_fn[i]["InterEatInt_n"] = "NA"  

        for dsfn in data_snack_fn:
            if "InterEatInt_n" in dsfn:
                data[dsfn["id"]]["InterEatInt_n"] = dsfn["InterEatInt_n"]
            if "Paradigm" in dsfn:
                data[dsfn["id"]]["Paradigm"] = dsfn["Paradigm"]


        # if key isnt there, add it in
        for sampledict in data:
            if 'IBI' not in sampledict:
                sampledict["IBI"] = "NA"
            if 'InterEatInt_f' not in sampledict:
                sampledict["InterEatInt_f"] = "NA"
            if 'InterEatInt_n' not in sampledict:
                sampledict["InterEatInt_n"] = "NA"
            if 'Paradigm' not in sampledict:
                sampledict["Paradigm"] = "NA"

        # create folder for output if it doesnt exist
        out_path = "/Users/barifuchs/Box/Bari_files/00_PennState/Bout_Project/Data/Intervals/"
        if not os.path.exists(out_path):
            os.makedirs(out_path)

        # write list of dictionaries to csv
        with open(out_path + prefix + '_intervals.csv', 'w') as csvfile:
            fc = csv.DictWriter(csvfile, fieldnames=['id','Paradigm','start_time','end_time','Annotation','category','Foodtype','chewtimes','IBI','InterEatInt_f','InterEatInt_n'])
            fc.writeheader()
            fc.writerows(data)