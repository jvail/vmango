from builtins import str
from builtins import range
from datetime import date, timedelta


######################################################################@

Month = {'janv' : 1, 'fev' : 2, 'mars' : 3,
         'avril' : 4, 'mai' : 5, 'juin' : 6,
         'juil' : 7, 'aout' : 8, 'sept' : 9,
         'oct' : 10, 'nov' : 11, 'dec' : 12 }

MonthEn = {'january' : 1, 'february' : 2, 'march' : 3,
         'april' : 4, 'may' : 5, 'june' : 6,
         'july' : 7, 'august' : 8, 'september' : 9,
         'october' : 10, 'november' : 11, 'december' : 12 }


MonthName = dict([(v,k) for k,v in list(Month.items())])
MonthEnName = dict([(v,k) for k,v in list(MonthEn.items())])


MonthOrder = list(range(6,13))+list(range(6))

def date_from_string(string):
    """From string = 'month.year', it return a date
    """
    m,y = string.split(".")
    date_ = date(2000+int(y), Month[m], 1)
    return date_

def todatetime(d): return d

######################################################################@

def month_difference(d1,d2):
    return (d1.year-d2.year)*12 + (d1.month-d2.month)

def week_difference(d1,d2):
    from math import ceil
    return ceil((d2 -d1).days/7.)

def get_date_from_week(year, week, weekday):
    from datetime import datetime
    assert 1 <= weekday <= 7
    return datetime.strptime(str(year)+'/'+str(week)+'/'+str(weekday%7),'%Y/%W/%w').date()

def get_week_from_date(date):
    return date.isocalendar()[1]
    
def get_weekday_from_date(date):
    return date.isocalendar()[2]

def get_semester(cdate):
    if cdate.month >= vegetative_cycle_begin(4).month : return 1
    else : return 2

def lastday_in_month(year, month):
    from calendar import monthrange
    return monthrange(year,month)[1]

def nbofdays_in_year(year):
    return (date(year,12,31)-date(year,1,1)).days

######################################################################@

def random_date_in_month(year, month):
    from random import randint
    return date(year,month, randint(1,lastday_in_month(year,month)))

######################################################################@

def date_range(begindate, enddate, daystep = 1):
    if enddate <= begindate: return []
    currentdate = begindate
    if type(daystep) == timedelta: delta = daystep
    else: delta = timedelta(days=daystep)
    res = []
    while currentdate < enddate :
        res.append(currentdate)
        currentdate += delta
    return res

def date_xrange(begindate, enddate, daystep = 1):
    currentdate = begindate
    if type(daystep) == timedelta: delta = daystep
    else: delta = timedelta(days=daystep)
    while currentdate  < enddate :
        yield currentdate
        currentdate += delta


def monthdate_range(begindate, enddate):
    if enddate <= begindate: return []
    currentmonth, currentyear = begindate.month, begindate.year
    endmonth, endyear = enddate.month, enddate.year
    res = []
    while currentyear < endyear or (currentmonth < endmonth and currentyear == endyear):
        res.append((currentyear, currentmonth))
        currentmonth += 1
        if currentmonth > 12:
            currentyear += 1
            currentmonth -= 12
    return res


def monthdate_xrange(begindate, enddate):
    currentmonth, currentyear = begindate.month, begindate.year
    endmonth, endyear = enddate.month, enddate.year
    while currentyear < endyear or (currentmonth < endmonth and currentyear == endyear):
        yield (currentyear, currentmonth)
        currentmonth += 1
        if currentmonth > 12:
            currentyear += 1
            currentmonth -= 12

def weekdate_range(begindate, enddate):
    currentweek, currentyear = get_week_from_date(begindate), begindate.year
    endweek, endyear = get_week_from_date(enddate), enddate.year
    maxweek = get_week_from_date(date(currentyear, 12, 28))
    res = []
    while currentyear < endyear or (currentweek < endweek and currentyear == endyear):
        res.append((currentyear,currentweek))
        currentweek += 1
        if currentweek > maxweek:
            currentyear += 1
            currentweek -= maxweek
            maxweek = get_week_from_date(date(currentyear, 12, 28))
    return res

def weekdate_xrange(begindate, enddate):
    currentweek, currentyear = get_week_from_date(begindate), begindate.year
    endweek, endyear = get_week_from_date(enddate), enddate.year
    maxweek = get_week_from_date(date(currentyear, 12, 28))
    while currentyear < endyear or (currentweek < endweek and currentyear == endyear):
        yield (currentyear,currentweek)
        currentweek += 1
        if currentweek > maxweek:
            currentyear += 1
            currentweek -= maxweek
            maxweek = get_week_from_date(date(currentyear, 12, 28))

######################################################################@

# Fred note : The actual cycle seems to start at beginning of June
def vegetative_cycle_begin(cycle):
    return date(2000+cycle-1,6,1)
    # return date(2000+cycle-1,7,1)

def vegetative_cycle_end(cycle):
   return date(2000+cycle,5,31)
   # return date(2000+cycle,6,30)

def in_vegetative_cycle(cdate, cycle):
    if type(cdate) != date: cdate = cdate.date()
    return vegetative_cycle_begin(cycle) <= cdate <= vegetative_cycle_end(cycle)


def get_vegetative_cycle(cdate):
    if type(cdate) != date: cdate = cdate.date()

    if cdate < vegetative_cycle_begin(4) : return 3
    else:
        gcycle = cdate.year-2000
        while True:
            if cdate <= vegetative_cycle_end(gcycle): 
                return gcycle
            else:
                gcycle += 1

def flowering_cycle_begin(cycle):
    return date(2000+cycle, 7, 1)

def flowering_cycle_end(cycle):
    return date(2000+cycle, 10, 31)

def in_flowering_cycle(cdate, cycle):
    if type(cdate) != date: cdate = cdate.date()
    return flowering_cycle_begin(cycle) <= cdate <= flowering_cycle_end(cycle)

def get_flowering_cycle(date):
    return date.year - 2000

def fruiting_cycle_begin(cycle):
    return date(2000+cycle, 11, 1)

def fruiting_cycle_end(cycle):
    return date(2000+cycle+1, 3, 31)

def in_fruiting_cycle(cdate, cycle):
    if type(cdate) != date: cdate = cdate.date()
    return fruiting_cycle_begin(cycle) <= cdate <= fruiting_cycle_end(cycle)

def get_fruiting_cycle(date):
    c = get_flowering_cycle(date)
    if in_fruiting_cycle(date,c) : return c
    else : return c - 1

#beg_end_period = {'E' : (7,8,9,10), 'I' : (11,12,1,2), 'L' : (3,4,5,6)}

######################################################################@

    

bloom_weeks = {}

def get_bloom_weeks(icycle):
    if not icycle in bloom_weeks:
        bloom_weeks_i = { 0 : (date(2000+icycle,7,1),get_date_from_week(2000+icycle,31,7)) }
        bloom_weeks_i.update(dict([(i, (get_date_from_week(2000+icycle,31+i,1), get_date_from_week(2000+icycle,31+i,7))) for i in range(1,13)]))
        bloom_weeks[icycle] = bloom_weeks_i
    return bloom_weeks[icycle]

def get_bloom_week(date, icycle = None):
    global bloom_weeks
    if icycle is None:
        icycle = get_flowering_cycle(date)
    bweeks = get_bloom_weeks(icycle)
    for periodid, period_beg_end  in list(bweeks.items()):
        if period_beg_end[0] <= date <= period_beg_end[1]:
            return periodid

harvest_weeks = {}

def get_harvest_weeks(icycle):
    if not icycle in harvest_weeks:
        harvest_weeks_i = dict([(i, (get_date_from_week(cyear,cweek,1), get_date_from_week(cyear,cweek,7))) for i, (cyear, cweek) in enumerate(weekdate_xrange(date(2000+icycle, 12, 15), date(2001+icycle, 3, 15)))])
        harvest_weeks[icycle] = harvest_weeks_i
    return harvest_weeks[icycle]    

def get_harvest_week(date, icycle = None):
    global harvest_weeks
    if icycle is None:
        icycle = get_fruiting_cycle(date)
    weeks = get_harvest_weeks(icycle)
    for periodid, period_beg_end  in list(weeks.items()):
        if period_beg_end[0] <= date <= period_beg_end[1]:
            return periodid

######################################################################@

