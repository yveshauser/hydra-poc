"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[2627],{3905:function(t,a,e){e.d(a,{Zo:function(){return g},kt:function(){return N}});var r=e(7294);function n(t,a,e){return a in t?Object.defineProperty(t,a,{value:e,enumerable:!0,configurable:!0,writable:!0}):t[a]=e,t}function i(t,a){var e=Object.keys(t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(t);a&&(r=r.filter((function(a){return Object.getOwnPropertyDescriptor(t,a).enumerable}))),e.push.apply(e,r)}return e}function l(t){for(var a=1;a<arguments.length;a++){var e=null!=arguments[a]?arguments[a]:{};a%2?i(Object(e),!0).forEach((function(a){n(t,a,e[a])})):Object.getOwnPropertyDescriptors?Object.defineProperties(t,Object.getOwnPropertyDescriptors(e)):i(Object(e)).forEach((function(a){Object.defineProperty(t,a,Object.getOwnPropertyDescriptor(e,a))}))}return t}function o(t,a){if(null==t)return{};var e,r,n=function(t,a){if(null==t)return{};var e,r,n={},i=Object.keys(t);for(r=0;r<i.length;r++)e=i[r],a.indexOf(e)>=0||(n[e]=t[e]);return n}(t,a);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(t);for(r=0;r<i.length;r++)e=i[r],a.indexOf(e)>=0||Object.prototype.propertyIsEnumerable.call(t,e)&&(n[e]=t[e])}return n}var m=r.createContext({}),p=function(t){var a=r.useContext(m),e=a;return t&&(e="function"==typeof t?t(a):l(l({},a),t)),e},g=function(t){var a=p(t.components);return r.createElement(m.Provider,{value:a},t.children)},d={inlineCode:"code",wrapper:function(t){var a=t.children;return r.createElement(r.Fragment,{},a)}},k=r.forwardRef((function(t,a){var e=t.components,n=t.mdxType,i=t.originalType,m=t.parentName,g=o(t,["components","mdxType","originalType","parentName"]),k=p(e),N=n,s=k["".concat(m,".").concat(N)]||k[N]||d[N]||i;return e?r.createElement(s,l(l({ref:a},g),{},{components:e})):r.createElement(s,l({ref:a},g))}));function N(t,a){var e=arguments,n=a&&a.mdxType;if("string"==typeof t||n){var i=e.length,l=new Array(i);l[0]=k;var o={};for(var m in a)hasOwnProperty.call(a,m)&&(o[m]=a[m]);o.originalType=t,o.mdxType="string"==typeof t?t:n,l[1]=o;for(var p=2;p<i;p++)l[p]=e[p];return r.createElement.apply(null,l)}return r.createElement.apply(null,e)}k.displayName="MDXCreateElement"},480:function(t,a,e){e.r(a),e.d(a,{assets:function(){return g},contentTitle:function(){return m},default:function(){return N},frontMatter:function(){return o},metadata:function(){return p},toc:function(){return d}});var r=e(7462),n=e(3366),i=(e(7294),e(3905)),l=["components"],o={sidebar_label:"Transactions Costs",sidebar_position:3},m="Transactions Costs",p={unversionedId:"transaction-cost",id:"transaction-cost",title:"Transactions Costs",description:"Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using arbitrary values and results are not fully deterministic and comparable to previous runs.",source:"@site/benchmarks/transaction-cost.md",sourceDirName:".",slug:"/transaction-cost",permalink:"/head-protocol/fr/benchmarks/transaction-cost",editUrl:"https://github.com/input-output-hk/hydra-poc/tree/master/docs/benchmarks/transaction-cost.md",tags:[],version:"current",sidebarPosition:3,frontMatter:{sidebar_label:"Transactions Costs",sidebar_position:3},sidebar:"defaultSidebar",previous:{title:"Benchmarks",permalink:"/head-protocol/fr/benchmarks/"}},g={},d=[{value:"Cost of Init Transaction",id:"cost-of-init-transaction",level:2},{value:"Cost of Commit Transaction",id:"cost-of-commit-transaction",level:2},{value:"Cost of CollectCom Transaction",id:"cost-of-collectcom-transaction",level:2},{value:"Cost of Close Transaction",id:"cost-of-close-transaction",level:2},{value:"Cost of Contest Transaction",id:"cost-of-contest-transaction",level:2},{value:"Cost of Abort Transaction",id:"cost-of-abort-transaction",level:2},{value:"Cost of FanOut Transaction",id:"cost-of-fanout-transaction",level:2}],k={toc:d};function N(t){var a=t.components,e=(0,n.Z)(t,l);return(0,i.kt)("wrapper",(0,r.Z)({},k,e,{components:a,mdxType:"MDXLayout"}),(0,i.kt)("h1",{id:"transactions-costs"},"Transactions Costs"),(0,i.kt)("p",null,"Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using ",(0,i.kt)("inlineCode",{parentName:"p"},"arbitrary")," values and results are not fully deterministic and comparable to previous runs."),(0,i.kt)("table",null,(0,i.kt)("thead",{parentName:"table"},(0,i.kt)("tr",{parentName:"thead"},(0,i.kt)("th",{parentName:"tr",align:"left"},"Metadata"),(0,i.kt)("th",{parentName:"tr",align:"left"}))),(0,i.kt)("tbody",{parentName:"table"},(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},(0,i.kt)("em",{parentName:"td"},"Generated at")),(0,i.kt)("td",{parentName:"tr",align:"left"},"2022-07-12 09:11:25.890659051 UTC")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},(0,i.kt)("em",{parentName:"td"},"Max. memory units")),(0,i.kt)("td",{parentName:"tr",align:"left"},"14000000.00")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},(0,i.kt)("em",{parentName:"td"},"Max. CPU units")),(0,i.kt)("td",{parentName:"tr",align:"left"},"10000000000.00")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},(0,i.kt)("em",{parentName:"td"},"Max. tx size (kB)")),(0,i.kt)("td",{parentName:"tr",align:"left"},"16384")))),(0,i.kt)("h2",{id:"cost-of-init-transaction"},"Cost of Init Transaction"),(0,i.kt)("table",null,(0,i.kt)("thead",{parentName:"table"},(0,i.kt)("tr",{parentName:"thead"},(0,i.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,i.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,i.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,i.kt)("th",{parentName:"tr",align:"right"},"% max CPU"))),(0,i.kt)("tbody",{parentName:"table"},(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"1"),(0,i.kt)("td",{parentName:"tr",align:"right"},"4832"),(0,i.kt)("td",{parentName:"tr",align:"right"},"12.23"),(0,i.kt)("td",{parentName:"tr",align:"right"},"4.88")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"2"),(0,i.kt)("td",{parentName:"tr",align:"right"},"5042"),(0,i.kt)("td",{parentName:"tr",align:"right"},"13.95"),(0,i.kt)("td",{parentName:"tr",align:"right"},"5.53")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"3"),(0,i.kt)("td",{parentName:"tr",align:"right"},"5241"),(0,i.kt)("td",{parentName:"tr",align:"right"},"15.73"),(0,i.kt)("td",{parentName:"tr",align:"right"},"6.21")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"5"),(0,i.kt)("td",{parentName:"tr",align:"right"},"5651"),(0,i.kt)("td",{parentName:"tr",align:"right"},"17.14"),(0,i.kt)("td",{parentName:"tr",align:"right"},"6.69")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"10"),(0,i.kt)("td",{parentName:"tr",align:"right"},"6678"),(0,i.kt)("td",{parentName:"tr",align:"right"},"27.72"),(0,i.kt)("td",{parentName:"tr",align:"right"},"10.78")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"45"),(0,i.kt)("td",{parentName:"tr",align:"right"},"13855"),(0,i.kt)("td",{parentName:"tr",align:"right"},"98.02"),(0,i.kt)("td",{parentName:"tr",align:"right"},"37.89")))),(0,i.kt)("h2",{id:"cost-of-commit-transaction"},"Cost of Commit Transaction"),(0,i.kt)("p",null," Currently only one UTxO per commit allowed (this is about to change soon)"),(0,i.kt)("table",null,(0,i.kt)("thead",{parentName:"table"},(0,i.kt)("tr",{parentName:"thead"},(0,i.kt)("th",{parentName:"tr",align:"left"},"UTxO"),(0,i.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,i.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,i.kt)("th",{parentName:"tr",align:"right"},"% max CPU"))),(0,i.kt)("tbody",{parentName:"table"},(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"1"),(0,i.kt)("td",{parentName:"tr",align:"right"},"5770"),(0,i.kt)("td",{parentName:"tr",align:"right"},"19.67"),(0,i.kt)("td",{parentName:"tr",align:"right"},"7.93")))),(0,i.kt)("h2",{id:"cost-of-collectcom-transaction"},"Cost of CollectCom Transaction"),(0,i.kt)("table",null,(0,i.kt)("thead",{parentName:"table"},(0,i.kt)("tr",{parentName:"thead"},(0,i.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,i.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,i.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,i.kt)("th",{parentName:"tr",align:"right"},"% max CPU"))),(0,i.kt)("tbody",{parentName:"table"},(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"1"),(0,i.kt)("td",{parentName:"tr",align:"right"},"13058"),(0,i.kt)("td",{parentName:"tr",align:"right"},"20.77"),(0,i.kt)("td",{parentName:"tr",align:"right"},"8.32")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"2"),(0,i.kt)("td",{parentName:"tr",align:"right"},"13450"),(0,i.kt)("td",{parentName:"tr",align:"right"},"37.31"),(0,i.kt)("td",{parentName:"tr",align:"right"},"15.12")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"3"),(0,i.kt)("td",{parentName:"tr",align:"right"},"13666"),(0,i.kt)("td",{parentName:"tr",align:"right"},"54.42"),(0,i.kt)("td",{parentName:"tr",align:"right"},"22.19")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"4"),(0,i.kt)("td",{parentName:"tr",align:"right"},"14164"),(0,i.kt)("td",{parentName:"tr",align:"right"},"79.02"),(0,i.kt)("td",{parentName:"tr",align:"right"},"32.43")))),(0,i.kt)("h2",{id:"cost-of-close-transaction"},"Cost of Close Transaction"),(0,i.kt)("table",null,(0,i.kt)("thead",{parentName:"table"},(0,i.kt)("tr",{parentName:"thead"},(0,i.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,i.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,i.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,i.kt)("th",{parentName:"tr",align:"right"},"% max CPU"))),(0,i.kt)("tbody",{parentName:"table"},(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"1"),(0,i.kt)("td",{parentName:"tr",align:"right"},"9256"),(0,i.kt)("td",{parentName:"tr",align:"right"},"7.59"),(0,i.kt)("td",{parentName:"tr",align:"right"},"2.99")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"2"),(0,i.kt)("td",{parentName:"tr",align:"right"},"9488"),(0,i.kt)("td",{parentName:"tr",align:"right"},"8.97"),(0,i.kt)("td",{parentName:"tr",align:"right"},"3.81")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"3"),(0,i.kt)("td",{parentName:"tr",align:"right"},"9690"),(0,i.kt)("td",{parentName:"tr",align:"right"},"10.19"),(0,i.kt)("td",{parentName:"tr",align:"right"},"4.44")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"5"),(0,i.kt)("td",{parentName:"tr",align:"right"},"9989"),(0,i.kt)("td",{parentName:"tr",align:"right"},"11.31"),(0,i.kt)("td",{parentName:"tr",align:"right"},"5.17")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"10"),(0,i.kt)("td",{parentName:"tr",align:"right"},"10880"),(0,i.kt)("td",{parentName:"tr",align:"right"},"16.04"),(0,i.kt)("td",{parentName:"tr",align:"right"},"7.75")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"30"),(0,i.kt)("td",{parentName:"tr",align:"right"},"14221"),(0,i.kt)("td",{parentName:"tr",align:"right"},"32.44"),(0,i.kt)("td",{parentName:"tr",align:"right"},"17.09")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"63"),(0,i.kt)("td",{parentName:"tr",align:"right"},"15513"),(0,i.kt)("td",{parentName:"tr",align:"right"},"38.28"),(0,i.kt)("td",{parentName:"tr",align:"right"},"14.50")))),(0,i.kt)("h2",{id:"cost-of-contest-transaction"},"Cost of Contest Transaction"),(0,i.kt)("table",null,(0,i.kt)("thead",{parentName:"table"},(0,i.kt)("tr",{parentName:"thead"},(0,i.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,i.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,i.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,i.kt)("th",{parentName:"tr",align:"right"},"% max CPU"))),(0,i.kt)("tbody",{parentName:"table"},(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"1"),(0,i.kt)("td",{parentName:"tr",align:"right"},"9331"),(0,i.kt)("td",{parentName:"tr",align:"right"},"8.16"),(0,i.kt)("td",{parentName:"tr",align:"right"},"3.34")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"2"),(0,i.kt)("td",{parentName:"tr",align:"right"},"9529"),(0,i.kt)("td",{parentName:"tr",align:"right"},"9.38"),(0,i.kt)("td",{parentName:"tr",align:"right"},"3.96")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"3"),(0,i.kt)("td",{parentName:"tr",align:"right"},"9661"),(0,i.kt)("td",{parentName:"tr",align:"right"},"9.72"),(0,i.kt)("td",{parentName:"tr",align:"right"},"4.24")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"5"),(0,i.kt)("td",{parentName:"tr",align:"right"},"10057"),(0,i.kt)("td",{parentName:"tr",align:"right"},"12.12"),(0,i.kt)("td",{parentName:"tr",align:"right"},"5.47")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"10"),(0,i.kt)("td",{parentName:"tr",align:"right"},"10881"),(0,i.kt)("td",{parentName:"tr",align:"right"},"16.01"),(0,i.kt)("td",{parentName:"tr",align:"right"},"7.73")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"30"),(0,i.kt)("td",{parentName:"tr",align:"right"},"14189"),(0,i.kt)("td",{parentName:"tr",align:"right"},"32.02"),(0,i.kt)("td",{parentName:"tr",align:"right"},"16.92")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"43"),(0,i.kt)("td",{parentName:"tr",align:"right"},"16374"),(0,i.kt)("td",{parentName:"tr",align:"right"},"42.92"),(0,i.kt)("td",{parentName:"tr",align:"right"},"23.08")))),(0,i.kt)("h2",{id:"cost-of-abort-transaction"},"Cost of Abort Transaction"),(0,i.kt)("table",null,(0,i.kt)("thead",{parentName:"table"},(0,i.kt)("tr",{parentName:"thead"},(0,i.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,i.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,i.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,i.kt)("th",{parentName:"tr",align:"right"},"% max CPU")))),(0,i.kt)("h2",{id:"cost-of-fanout-transaction"},"Cost of FanOut Transaction"),(0,i.kt)("p",null," Involves spending head output and burning head tokens. Uses ada-only UTxO for better comparability."),(0,i.kt)("table",null,(0,i.kt)("thead",{parentName:"table"},(0,i.kt)("tr",{parentName:"thead"},(0,i.kt)("th",{parentName:"tr",align:"left"},"UTxO"),(0,i.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,i.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,i.kt)("th",{parentName:"tr",align:"right"},"% max CPU"))),(0,i.kt)("tbody",{parentName:"table"},(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"1"),(0,i.kt)("td",{parentName:"tr",align:"right"},"13488"),(0,i.kt)("td",{parentName:"tr",align:"right"},"10.26"),(0,i.kt)("td",{parentName:"tr",align:"right"},"4.46")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"2"),(0,i.kt)("td",{parentName:"tr",align:"right"},"13525"),(0,i.kt)("td",{parentName:"tr",align:"right"},"11.78"),(0,i.kt)("td",{parentName:"tr",align:"right"},"5.34")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"3"),(0,i.kt)("td",{parentName:"tr",align:"right"},"13625"),(0,i.kt)("td",{parentName:"tr",align:"right"},"13.93"),(0,i.kt)("td",{parentName:"tr",align:"right"},"6.47")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"5"),(0,i.kt)("td",{parentName:"tr",align:"right"},"13693"),(0,i.kt)("td",{parentName:"tr",align:"right"},"17.27"),(0,i.kt)("td",{parentName:"tr",align:"right"},"8.35")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"10"),(0,i.kt)("td",{parentName:"tr",align:"right"},"13813"),(0,i.kt)("td",{parentName:"tr",align:"right"},"24.25"),(0,i.kt)("td",{parentName:"tr",align:"right"},"12.50")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"50"),(0,i.kt)("td",{parentName:"tr",align:"right"},"15251"),(0,i.kt)("td",{parentName:"tr",align:"right"},"84.88"),(0,i.kt)("td",{parentName:"tr",align:"right"},"47.60")),(0,i.kt)("tr",{parentName:"tbody"},(0,i.kt)("td",{parentName:"tr",align:"left"},"60"),(0,i.kt)("td",{parentName:"tr",align:"right"},"15546"),(0,i.kt)("td",{parentName:"tr",align:"right"},"99.74"),(0,i.kt)("td",{parentName:"tr",align:"right"},"56.26")))))}N.isMDXComponent=!0}}]);