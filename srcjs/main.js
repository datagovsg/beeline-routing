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

  Vue.component('route-line', {
    template: `
<google-polyline
  :path="route.path" :options="options" v-if="route.count > 15">
  </google-polyline>
    `,
    data() {
      return {options: {}}
    },
    props: ['route'],
    computed: {
    },
    created() {
      loaded.then(() => {
        this.options = {
          strokeOpacity: 0.3,
          strokeColor: '#FF0000',
          strokeWeight: this.route.count / 10,
          zIndex: 1000
        }
      })
    }
  })
  Vue.component('request-line', {
    template: `
<google-polyline
  :path="path" :options="options">
  </google-polyline>
    `,
    data() {
      return {options: {}}
    },
    props: ['request'],
    computed: {
      path() {
        debugger;
        return [
          this.request.start,
          this.request.end
        ]
      }
    },
    created() {
      loaded.then(() => {
        this.options = {
          strokeOpacity: 0.3,
          strokeColor: '#CCCCCC'
        }
      })
    }
  })

  var styles = new Vue()

  new Vue({
    el: document.body,
    data: {
      styles,
      busStops: [],
      requests: [],
      routes: []
    },
    computed: {
      routeStyle: {}
    },
    created() {
      this.$http.get('./routes.json')
        .then((response) => response.json())
        .then(r => this.routes = r)
      this.$http.get('./problem.json')
        .then((response) => response.json())
        .then(r => this.requests = r)
      // this.$http.get('./busStops.json')
      //   .then((response) => response.json())
      //   .then(r => this.busStops = r)
    }
  })

})
