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
<google-polyline :path="path" :options="pathOptions">
</google-polyline>

<google-circle v-for="stop in route.stops"
  v-if="stop.numBoard"
  :center="stop"
  :radius="stop.numBoard * radiusScale"
  :options="boardCircleOptions"
  @g-mouseover="showPopup(stop)"
>
</google-circle>
<google-circle v-for="stop in route.stops"
 v-if="stop.numAlight"
 :center="stop"
 :radius="stop.numAlight * radiusScale"
 :options="alightCircleOptions"
 @g-mouseover="showPopup(stop)"
>
</google-circle>
<google-info-window v-if="stop" :position="stop"
    :opened.sync="popupShown">
  {{stop.description}}
  <br>
  Boarding: {{stop.numBoard}}
  <br>
  Alighting: {{stop.numAlight}}
</google-info-window>
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
      }
    },
    methods: {
      showPopup(stop) {
        this.stop = stop;
      }
    },
    created() {
    }
  })

  var styles = new Vue()

  new Vue({
    el: document.body,
    data: {
      styles,
      busStops: [],
      requests: [],
      routes: [],
      selectedRoute: null,
    },
    computed: {
      routeStyle: {}
    },
    created() {
      this.refresh();
    },
    methods: {
      refresh() {
        this.$http.get('/currentRoutes')
          .then(response => response.json())
          .then(routes => {
            // Decorate with additional data
            for (let route of routes) {
              route.totalCount = _.sumBy(route.stops, r => r.numBoard)
            }

            this.routes = routes
          })
      },
      selectRoute(route) {
        this.selectedRoute = route;
      }
    }
  })

})
