const Vue = require('vue');
const VueResource = require('vue-resource');
import proj4 from 'proj4';
import {loaded, load, Polyline, Map, Marker, Circle, InfoWindow} from 'vue-google-maps'
import _ from 'lodash';

proj4.defs([
    ['epsg:3414',
         '+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs '],
         ]);

var toSVY = proj4('epsg:3414').forward;
var toWGS = proj4('epsg:3414').inverse;

document.addEventListener('DOMContentLoaded', () => {
  Vue.use(VueResource)

  load({key: '<<< GOOGLE MAPS KEY REDACTED >>>'});

  Vue.component('google-map', Map);
  Vue.component('google-circle', Circle);
  Vue.component('google-polyline', Polyline);
  Vue.component('google-marker', Marker);
  Vue.component('google-info-window', InfoWindow);
  Vue.component('window-frame', Window);

  Vue.component('route-suggestion', {
    template: `
<google-polyline :path="route.path" :options="pathOptions"
  @g-mouseover="showPopup($event, stop)"
  @g-mouseout="mouseout($event, stop)"
>
</google-polyline>

<template v-if="selected" v-for="request in route.requests">
  <google-marker v-if="selected" v-for="request in route.requests" :position="request.start" :icon="requestOptions.icon"></google-marker>
  <google-marker v-if="selected" v-for="request in route.requests" :position="request.end" :icon="requestOptions.icon"></google-marker>
</template>

<google-circle v-for="stop in route.stops"
  v-if="selected && stop.numBoard"
  :center="stop"
  :radius="computeRadius(stop.numBoard)"
  :options="boardCircleOptions"
  @g-mouseover="showPopup($event, stop)"
  @g-mouseout="mouseout($event, stop)"
  @g-click="click($event, stop)"
>
</google-circle>
<google-circle v-for="stop in route.stops"
  v-if="selected && stop.numAlight"
  :center="stop"
  :radius="computeRadius(stop.numAlight)"
  :options="alightCircleOptions"
  @g-mouseover="showPopup($event, stop)"
  @g-mouseout="mouseout($event, stop)"
  @g-click="click($event, stop)"
>
</google-circle>
    `,
    data() {
      return {
        stop: null,
        popupShown: false,
        radiusScale: 100
      }
    },
    props: ['route', 'selected'],
    computed: {
      path() {
        return this.route.stops.map(s => _.pick(s, ['lat', 'lng']))
      },
      pathOptions() {
        return {
          strokeOpacity: this.selected ? 1 : 0.1,
          strokeColor: '#000000',
          strokeWeight: this.selected ? 3 : 0.5,
          zIndex: 1000
        }
      },
      alightCircleOptions() {
        return {
          strokeColor: '#000000',
          strokeOpacity: 0.3,
          strokeWeight: 1,
          fillOpacity: this.selected ? 1 : 0.3,
          fillColor: '#990000',
        }
      },
      boardCircleOptions() {
        return {
          strokeColor: '#000000',
          strokeOpacity: 0.3,
          strokeWeight: 1,
          fillOpacity: this.selected ? 1 : 0.3,
          fillColor: '#009900',
        }
      },
      requestOptions() {
        return {
          icon: {
            url: '/static/spot.png',
            anchor: new google.maps.Point(5,5),
          },
          zIndex: google.maps.MAX_ZINDEX + 1
        }
      },
    },
    methods: {
      showPopup($event, stop) {
        this.stop = stop;
        this.popupShown = true;
        this.$emit('mouseover', stop)
      },
      click($event, stop) {
        this.$emit('click', stop)
      },
      mouseout($event, stop) {
        this.$emit('mouseout', stop)
      },
      computeRadius(radius) {
        return Math.sqrt(radius) * this.radiusScale
      }
    },
    created() {
    }
  })

  Vue.component('region-selection', {
    props: ['regions', 'selected'],

    template: `
      <google-circle v-for="region in regions" track-by="$index" :center.sync="region.center" :radius.sync="region.radius"
                     :editable="true" :draggable="true" :options="circleOptions(region)" @g-click="selectRegion(region)"></google-circle>
    `,

    data() {
      return {}
    },

    methods: {
      selectRegion(region) {
        this.selectedRegion = region
      },

      circleOptions(region) {
        return {
          strokeColor: "#000000",
          strokeWeight: 1,
          fillColor: (region == this.selectedRegion) ? "#FF0000" : "#000000",
          fillOpacity: 0.1
        }
      }
    },
  })

  var styles = new Vue()

  Vue.filter('formatTime', (time) => {
    var d = new Date;
    d.setUTCHours(0,0,0,0)
    d.setTime(d.getTime() + parseInt(time))

    return d.toISOString().substr(11,5)
  })

  new Vue({
    el: document.body,
    data: {
      styles,
      busStops: [],
      requests: [],
      routes: [],
      selectedRoute: null,
      hoveredRoute: null,

      popupShown: false,
      hoveredStop: null,

      regions: [],
      selectedRegion: null,

      routingStarted: false,

      time: 16
    },
    computed: {
      routeStyle: {}
    },
    created() {
    },
    methods: {
      refresh() {
        this.$http.get('/routing/current')
          .then(response => response.json())
          .then(routes => {
            // Decorate with additional data
            for (let route of routes) {
              route.totalCount = _.sumBy(route.stops, r => r.numBoard)
            }

            this.routes = _.sortBy(routes, r => -r.totalCount)
          })
      },
      selectRoute(route) {
        this.selectedRoute = route;
      },
      hoverRoute($event, route) {
        this.hoveredStop = $event
        this.popupShown = true;
        this.hoveredRoute = route
      },
      startRouting() {
        this.$http.post('/routing/start', {
            time: this.time,
            regions: this.regions.map(r => ({
              lat: r.center.lat,
              lng: r.center.lng,
              radius: r.radius,
            }))
          })
          .then((result) => {
            console.log('Start routing returned ', result)
//            this.refresh().then(() => this.startPolling());
          })
          .catch((result) => {
            this.routingStarted = false;
          })

        this.routingStarted = true;
      },
      stopRouting() {
        this.$http.post('/routing/stop')
          .then((result) => {
            console.log('Stop routing returned ', result)
            this.routingStarted = false;
          })
      },
      addRegion() {
        this.regions.push({
          center: {
            lat: 1.38 + Math.random() * 0.1 - 0.05,
            lng: 103.8 + Math.random() * 0.1 - 0.05,
          },
          radius: 5000
        })
      },
      selectRegion(region) {
        this.selectedRegion = region;
      },
      removeRegion(region) {
        this.regions.splice(this.regions.indexOf(region))
      }
    }
  })

})
