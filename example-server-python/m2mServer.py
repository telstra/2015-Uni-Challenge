"""
    M2M Server

    Sample M2M Server code for the university challenge.
    This code is based off of the Flask tutorial by Armin Ronacher, found here:
       http://flask.pocoo.org/docs/tutorial/

    :license: BSD, see LICENSE for more details.
"""

#################################
#            Imports     	#
#################################

from sqlite3 import dbapi2 as sqlite3
from flask import Flask, request, session, g, redirect, url_for, abort, \
     render_template, flash, jsonify, make_response, Response
from flask.ext.googlemaps import GoogleMaps, Map
import json
from functools import wraps


app = Flask(__name__)
GoogleMaps(app)

# Load default config or override config from an environment variable, if it exists
app.config.update(dict(
    DATABASE='M2M.db',
    DEBUG=True,
    SECRET_KEY='someRandomKey',
    USERNAME='m2m',
    PASSWORD='challenge'
))
app.config.from_envvar('M2M_SETTINGS', silent=True)

#################################
#       Database methods        #
#################################


def connect_db():
    """Connects to the database defined in config."""
    rv = sqlite3.connect(app.config['DATABASE'])
    rv.row_factory = sqlite3.Row
    return rv


def init_db():
    """Creates the database tables."""
    with app.app_context():
        db = get_db()
        with app.open_resource('schema.sql', mode='r') as f:
            db.cursor().executescript(f.read())
        db.commit()


def get_db():
    """Opens a new database connection if there is none yet for the
    current application context.
    """
    if not hasattr(g, 'sqlite_db'):
        g.sqlite_db = connect_db()
    return g.sqlite_db


@app.teardown_appcontext
def close_db(error):
    """Closes the database again at the end of the request."""
    if hasattr(g, 'sqlite_db'):
        g.sqlite_db.close()



#######################################
#        Basic auth functions     	   #
#######################################

def check_auth(username, password):
    """This function is called to check if a username /
    password combination is valid.
    """
    return username == app.config['USERNAME'] and password == app.config['PASSWORD']

def authenticate():
    """Sends a 401 response that enables basic auth"""
    return Response(
    'Could not verify your access level for that URL.\n'
    'You have to login with proper credentials', 401,
    {'WWW-Authenticate': 'Basic realm="Login Required"'})

def requires_auth(f):
    @wraps(f)
    def decorated(*args, **kwargs):
        auth = request.authorization
        if not auth or not check_auth(auth.username, auth.password):
            return authenticate()
        return f(*args, **kwargs)
    return decorated

#################################
#       Route definitions       #
#################################

@app.route('/records')
def show_records():
    """ Returns a page showing all the records currently in the DB, rendered as a
    nice human readable list. 
    """
    db = get_db()
    cur = db.execute('select imei, imsi, time_stamp, cpu_ID, display_string, lat, lon from records order by id desc')
    records = cur.fetchall()
    return render_template('show_records.html', records=records)

@app.route('/')
def show_gsg():
    """ Returns a page showing all the records currently in the DB, rendered as a
    nice human readable list. 
    """
    db = get_db()
    cur = db.execute('select imei, imsi, time_stamp, cpu_ID, display_string, lat, lon from records order by id desc')
    records = cur.fetchall()
    return render_template('show_records.html', records=records)

@app.route('/map')
def show_map_unique_cpu():
    return render_template('map_unique.html')

# This method uses the python google-maps api to add basic markers to the google map
@app.route('/map_python')
def show_map():
    db = get_db()
    cur = db.execute('select imei, imsi, time_stamp, cpu_ID, display_string, lat, lon from records order by id desc')
    records = cur.fetchall()
    
    # create list of markers from the lat/lng values for each records in the DB
    markerList = []
    for item in records:
        markerList.append((float(item[5]), float(item[6])))
    
    # create the map object
    mymap = Map(
        identifier="mymap",
        lat=-28,
        lng=135,
        zoom=4,
        markers=markerList,
	style="height:600px;width:800px;"
    )

    return render_template('map_python.html', mymap=mymap)

# Handles the various methods for /api/position
# GET: returns JSON list of all the records already in the DB
# POST: accepts positions details sent from the client in JSON format, and inserts them into the SQLite db.
@app.route('/api/position', methods=['GET', 'POST', 'OPTIONS'])
def add_record():
    keys = ('imei','imsi','time_stamp','cpu_ID','display_string','lat', 'lon')
    db = get_db()
    if request.method == 'POST':
        jsonData = request.get_json(force=True)
        print "REQ DATA", jsonData
        db.execute('insert into records (imei, imsi, time_stamp, cpu_ID, display_string, lat, lon) values (?, ?, ?, ?, ?, ?, ?)',
        [jsonData['imei'], jsonData['imsi'],jsonData['timestampUTC'], jsonData['cpuID'],jsonData['displayString'], jsonData['latitude'], jsonData['longitude']])
        db.commit()
        return '200 OK'
    
    if request.method == 'GET':
        cur = db.execute('select imei, imsi, time_stamp, cpu_ID, display_string, lat, lon from records order by id desc')
        records = cur.fetchall()
        
        if len(records) > 0:
            outputList = []
            for record in records:
                outputList.append(dict(zip(keys, record)))
                
            # craft response    
            resp2 = Response(json.dumps(outputList),  mimetype='application/json')			
            resp2.headers.add('Access-Control-Allow-Origin', '*')
            return resp2
        else:
            return 'no records posted'

    if request.method == 'OPTIONS':
            resp = make_response()          
            resp.headers.add('Access-Control-Allow-Headers', 'origin, content-type, accept')
            resp.headers.add('Access-Control-Allow-Origin', '*')
            return resp


if __name__ == '__main__':
    # init_db() 
	# Uncommenting the above line will make the server reinitialise the db each time it's run,
    # removing any previous records, leave commented for a persistent DB
    
    app.run(host='0.0.0.0', port=80)  # Make server publicly available on port 80
    #app.run() # Make the server only available locally on port 5000 (127.0.0.1:5000)
