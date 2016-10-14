

var L = require('leaflet')

document.addEventListener('DOMContentLoaded', () => {
  var map = L.map('map').setView([1.38, 103.8], 15);

  L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png', {
      attribution: '&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors'
  }).addTo(map);

  setTimeout(() => {
    document.querySelector('.leaflet-container').style.cursor='crosshair'
  }, 200)

  map.on('click', (event) => {
    if (rrQueue[0]) {
      rrQueue[0][0](event.latlng);
      rrQueue.pop();
    }
  })
  document.getElementById('cancelButton').addEventListener('click', (event) => {
    if (rrQueue[0]) {
      rrQueue[0][1]();
      rrQueue.pop();
    }
  })


  function getNext() {
    return new Promise((resolve, reject) => {
      var req = new XMLHttpRequest();

      req.open('GET', 'http://localhost:9999/next')
      req.addEventListener('load', function() {
        return resolve(JSON.parse(this.responseText))
      })
      req.addEventListener('error', function () { reject(this) })
      req.send();
    })
  }

  function putNext(data) {
    return new Promise((resolve, reject) => {
      var req = new XMLHttpRequest();

      req.open('POST', 'http://localhost:9999/put')
      // req.submittedData = JSON.stringify(data);
      req.setRequestHeader('content-type', 'application/json')
      req.addEventListener('load', function() {
        return resolve(JSON.parse(this.responseText))
      })
      req.addEventListener('error', function () { reject(this) })
      req.send(JSON.stringify(data));
    })
  }

  var rrQueue = [];
  function getClickLocation() {
    return new Promise((resolve, reject) => {
      rrQueue.push([resolve, reject])
    })
  }

  async function requestAndPost(s) {
    document.getElementById("description").innerHTML=s.Description

    map.setView([s.Latitude, s.Longitude], 18)
    var marker = L.marker([s.Latitude, s.Longitude], {
      icon: new L.Icon({
        iconUrl: './marker.png',
        iconAnchor: [16, 37],
      })
    });
    marker.addTo(map)

    var l1 = await getClickLocation();

    console.log(s)

    var line;

    line = L.polyline(
      [
        [l1.lat, l1.lng],
        [l1.lat + 0.00001, l1.lng + 0.00001],
      ],
      {color: 'black', weight: 5}
    )
    line.addTo(map)

    var l2 = await getClickLocation();
    map.removeControl(line);

    line = L.polyline(
      [
        [l1.lat, l1.lng],
        [l2.lat, l2.lng],
      ],
      {color: 'black', strokeWeight: 5, opacity: 1.0}
    )
    line.addTo(map)

    await getClickLocation()
    map.removeControl(line);

    s.heading = [
      [l1.lat, l1.lng],
      [l2.lat, l2.lng],
    ]

    await putNext(s);
    map.removeControl(marker)
  }

  ;(async () => {
    var s = await getNext();
    while (true) {
      try {
        await requestAndPost(s);
        s = await getNext();
      } catch (err) {}
    }
  })()
})
