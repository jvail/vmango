from datetime import datetime, date


Month = {'janv' : 1, 'fev' : 2, 'mars' : 3,
         'avril' : 4, 'mai' : 5, 'juin' : 6,
         'juil' : 7, 'aout' : 8, 'sept' : 9,
         'oct' : 10, 'nov' : 11, 'dec' : 12 }

def date_from_string(string):
    """From string = 'month.year', it return a date
    """
    m,y = string.split(".")
    date_ = date(2000+int(y), Month[m], 1)
    return date_


def month_difference(d1,d2):
    return (d1.year-d2.year)*12 + (d1.month-d2.month)

def week_difference(d1,d2):
    from math import ceil
    return ceil((d2 -d1).days/7.)

# Fred note : The actual cycle seems to start at begining of June
def cycle_begining(cycle):
    return date(2000+cycle-1,6,1)
    # return date(2000+cycle-1,7,1)

def cycle_end(cycle):
   return date(2000+cycle,5,31)
   # return date(2000+cycle,6,30)

def in_cycle(date, cycle):
    return cycle_begining(cycle) <= date <= cycle_end(cycle)


def get_cycle(date):
    if date < cycle_begining(4) : return 3
    elif date < cycle_begining(5) : return 4
    else : return 5


#beg_end_period = {'E' : (7,8,9,10), 'I' : (11,12,1,2), 'L' : (3,4,5,6)}


bloom_weeks_04 = {
0 : (datetime(2004,7,1),datetime(2004,8,7)),
1 : (datetime(2004,8,8),datetime(2004,8,14)),
2 : (datetime(2004,8,15),datetime(2004,8,21)),
3 : (datetime(2004,8,22),datetime(2004,8,28)),
4 : (datetime(2004,8,29),datetime(2004,9,4)),
5 : (datetime(2004,9,5),datetime(2004,9,11)),
6 : (datetime(2004,9,12),datetime(2004,9,18)),
7 : (datetime(2004,9,19),datetime(2004,9,25)),
8 : (datetime(2004,9,26),datetime(2004,10,2)),
9 : (datetime(2004,10,3),datetime(2004,10,9)),
10 : (datetime(2004,10,10),datetime(2004,10,16)),
11 : (datetime(2004,10,17),datetime(2004,10,23)),
12 : (datetime(2004,10,24),datetime(2004,10,30))  }

bloom_weeks_05 = {
0 : (datetime(2005,7,1),datetime(2005,8,7)),
1 : (datetime(2005,8,8),datetime(2005,8,14)),
2 : (datetime(2005,8,15),datetime(2005,8,21)),
3 : (datetime(2005,8,22),datetime(2005,8,28)),
4 : (datetime(2005,8,29),datetime(2005,9,4)),
5 : (datetime(2005,9,5),datetime(2005,9,11)),
6 : (datetime(2005,9,12),datetime(2005,9,18)),
7 : (datetime(2005,9,19),datetime(2005,9,25)),
8 : (datetime(2005,9,26),datetime(2005,10,2)),
9 : (datetime(2005,10,3),datetime(2005,10,9)),
10 : (datetime(2005,10,10),datetime(2005,10,16)),
11 : (datetime(2005,10,17),datetime(2005,10,23)),
12 : (datetime(2005,10,24),datetime(2005,10,30))  }

bloom_weeks = {4 : bloom_weeks_04, 5 : bloom_weeks_05}






