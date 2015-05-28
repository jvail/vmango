from datetime import date, timedelta


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

# Fred note : The actual cycle seems to start at beginning of June
def cycle_begin(cycle):
    return date(2000+cycle-1,6,1)
    # return date(2000+cycle-1,7,1)

def cycle_end(cycle):
   return date(2000+cycle,5,31)
   # return date(2000+cycle,6,30)

def in_cycle(date, cycle):
    return cycle_begin(cycle) <= date <= cycle_end(cycle)


def get_cycle(date):
    if date < cycle_begin(4) : return 3
    elif date < cycle_begin(5) : return 4
    else : return 5

def flowering_cycle_begin(cycle):
    return date(2000+cycle, 6, 15)

def flowering_cycle_end(cycle):
    return date(2000+cycle, 10, 31)

def in_flowering_cycle(date, cycle):
    return flowering_cycle_begin(cycle) <= date <= flowering_cycle_end(cycle)

def get_flowering_cycle(date):
    return date.year - 2000


def monthdate_range(begindate, enddate):
    if enddate <= begindate: return []
    currentmonth, currentyear = begindate.month, begindate.year
    endmonth, endyear = enddate.month, enddate.year
    res = []
    while currentyear < endyear or (currentmonth < endmonth and currentyear == endyear):
        res.append((currentmonth, currentyear))
        currentmonth += 1
        if currentmonth > 12:
            currentyear += 1
            currentmonth -= 12
    return res


def monthdate_xrange(begindate, enddate):
    currentmonth, currentyear = begindate.month, begindate.year
    endmonth, endyear = enddate.month, enddate.year
    while currentyear < endyear or (currentmonth < endmonth and currentyear == endyear):
        yield (currentmonth, currentyear)
        currentmonth += 1
        if currentmonth > 12:
            currentyear += 1
            currentmonth -= 12

def date_range(begindate, enddate, daystep = 1):
    if enddate <= begindate: return []
    currentdate = begindate
    delta = timedelta(days=daystep)
    res = []
    while currentdate < enddate :
        res.append(currentdate)
        currentdate += delta
    return res

def date_xrange(begindate, enddate, daystep = 1):
    currentdate = begindate
    delta = timedelta(days=daystep)
    while currentdate  < enddate :
        yield currentdate
        currentdate += delta

#beg_end_period = {'E' : (7,8,9,10), 'I' : (11,12,1,2), 'L' : (3,4,5,6)}


bloom_weeks_04 = {
0 : (date(2004,7,1),date(2004,8,7)),
1 : (date(2004,8,8),date(2004,8,14)),
2 : (date(2004,8,15),date(2004,8,21)),
3 : (date(2004,8,22),date(2004,8,28)),
4 : (date(2004,8,29),date(2004,9,4)),
5 : (date(2004,9,5),date(2004,9,11)),
6 : (date(2004,9,12),date(2004,9,18)),
7 : (date(2004,9,19),date(2004,9,25)),
8 : (date(2004,9,26),date(2004,10,2)),
9 : (date(2004,10,3),date(2004,10,9)),
10 : (date(2004,10,10),date(2004,10,16)),
11 : (date(2004,10,17),date(2004,10,23)),
12 : (date(2004,10,24),date(2004,10,30))  }

bloom_weeks_05 = {
0 : (date(2005,7,1),date(2005,8,7)),
1 : (date(2005,8,8),date(2005,8,14)),
2 : (date(2005,8,15),date(2005,8,21)),
3 : (date(2005,8,22),date(2005,8,28)),
4 : (date(2005,8,29),date(2005,9,4)),
5 : (date(2005,9,5),date(2005,9,11)),
6 : (date(2005,9,12),date(2005,9,18)),
7 : (date(2005,9,19),date(2005,9,25)),
8 : (date(2005,9,26),date(2005,10,2)),
9 : (date(2005,10,3),date(2005,10,9)),
10 : (date(2005,10,10),date(2005,10,16)),
11 : (date(2005,10,17),date(2005,10,23)),
12 : (date(2005,10,24),date(2005,10,30))  }

bloom_weeks = {4 : bloom_weeks_04, 5 : bloom_weeks_05}


def get_bloom_week(date, icycle = None):
    if icycle is None:
        icycle = get_cycle(date)
    bweeks = bloom_weeks[icycle]
    for periodid, period_beg_end  in bweeks.items():
        if period_beg_end[0] <= date <= period_beg_end[1]:
            return periodid

