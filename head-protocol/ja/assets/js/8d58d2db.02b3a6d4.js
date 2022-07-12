"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[5325],{9257:function(e,t,a){a.d(t,{Z:function(){return p}});var n=a(7294),i=a(1736),o="terminalWindow_wGrl",d="terminalWindowHeader_o9Cs",r="terminalWindowBody_tzdS",m="buttons_IGLB",l="dot_fGZE";function p(e){var t=e.children,a=e.minHeight,p="string"==typeof t?n.createElement(i.Z,null,t):t;return n.createElement("div",{className:o,style:{minHeight:a}},n.createElement("div",{className:d},n.createElement("div",{className:m},n.createElement("span",{className:l,style:{background:"#f25f58"}}),n.createElement("span",{className:l,style:{background:"#fbbe3c"}}),n.createElement("span",{className:l,style:{background:"#58cb42"}}))),n.createElement("div",{className:r},p))}},7500:function(e,t,a){a.r(t),a.d(t,{assets:function(){return s},contentTitle:function(){return l},default:function(){return u},frontMatter:function(){return m},metadata:function(){return p},toc:function(){return c}});var n=a(7462),i=a(3366),o=(a(7294),a(3905)),d=a(9257),r=["components"],m={sidebar_position:2},l="Docker\u3067\u4f7f\u7528",p={unversionedId:"getting-started/demo/with-docker",id:"getting-started/demo/with-docker",title:"Docker\u3067\u4f7f\u7528",description:"Hydra Head\u30d7\u30ed\u30c8\u30b3\u30eb\u306e\u30c7\u30e2\u3092\u884c\u3046\u305f\u3081\u306e\u6a19\u6e96\u7684\u306a\u30c7\u30e2\u30bb\u30c3\u30c8\u30a2\u30c3\u30d7\u3067\u3059\u3002",source:"@site/i18n/ja/docusaurus-plugin-content-docs/current/getting-started/demo/with-docker.md",sourceDirName:"getting-started/demo",slug:"/getting-started/demo/with-docker",permalink:"/head-protocol/ja/docs/getting-started/demo/with-docker",editUrl:"https://github.com/input-output-hk/hydra-poc/tree/master/docs/docs/getting-started/demo/with-docker.md",tags:[],version:"current",sidebarPosition:2,frontMatter:{sidebar_position:2},sidebar:"defaultSidebar",previous:{title:"\u30c7\u30e2",permalink:"/head-protocol/ja/docs/getting-started/demo/"},next:{title:"\u5b9f\u884c\u30d5\u30a1\u30a4\u30eb\u3067\u4f7f\u7528(Docker\u306a\u3057)",permalink:"/head-protocol/ja/docs/getting-started/demo/without-docker"}},s={},c=[{value:"\u30cd\u30c3\u30c8\u30ef\u30fc\u30af\u306e\u8a2d\u5b9a",id:"\u30cd\u30c3\u30c8\u30ef\u30fc\u30af\u306e\u8a2d\u5b9a",level:2},{value:"\u30cd\u30c3\u30c8\u30ef\u30fc\u30af\u306e\u69cb\u7bc9",id:"\u30cd\u30c3\u30c8\u30ef\u30fc\u30af\u306e\u69cb\u7bc9",level:2},{value:"\u30af\u30e9\u30a4\u30a2\u30f3\u30c8\u306e\u5b9f\u884c",id:"\u30af\u30e9\u30a4\u30a2\u30f3\u30c8\u306e\u5b9f\u884c",level:2}],k={toc:c};function u(e){var t=e.components,a=(0,i.Z)(e,r);return(0,o.kt)("wrapper",(0,n.Z)({},k,a,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h1",{id:"docker\u3067\u4f7f\u7528"},"Docker\u3067\u4f7f\u7528"),(0,o.kt)("blockquote",null,(0,o.kt)("p",{parentName:"blockquote"},"Hydra Head\u30d7\u30ed\u30c8\u30b3\u30eb\u306e\u30c7\u30e2\u3092\u884c\u3046\u305f\u3081\u306e\u6a19\u6e96\u7684\u306a\u30c7\u30e2\u30bb\u30c3\u30c8\u30a2\u30c3\u30d7\u3067\u3059\u3002")),(0,o.kt)("p",null,"\u30c7\u30e2\u306e\u69cb\u6210:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"\u4e92\u3044\u306b\u76f4\u63a5\u63a5\u7d9a\u3055\u308c\u305f3\u3064\u306eHydra\u30ce\u30fc\u30c9\u304b\u3089\u306a\u308b\u30af\u30e9\u30b9\u30bf\u3067\u3001\u305d\u308c\u305e\u308c\u304c3\u3064\u306eHydra\u30af\u30ec\u30c7\u30f3\u30b7\u30e3\u30eb ",(0,o.kt)("inlineCode",{parentName:"li"},"alice"),", ",(0,o.kt)("inlineCode",{parentName:"li"},"bob"),", ",(0,o.kt)("inlineCode",{parentName:"li"},"carol")," \u306e\u3044\u305a\u308c\u304b\u306b\u30a2\u30af\u30bb\u30b9"),(0,o.kt)("li",{parentName:"ul"},"\u30ed\u30fc\u30ab\u30eb\u958b\u767a\u30cd\u30c3\u30c8\u3068\u3057\u3066\u7a3c\u50cd\u3059\u308b\u5358\u4e00\u306e\u30d6\u30ed\u30c3\u30af\u751f\u6210\u30ab\u30eb\u30c0\u30ce\u30ce\u30fc\u30c9"),(0,o.kt)("li",{parentName:"ul"},"\u30e1\u30c8\u30ea\u30af\u30b9\u53ce\u96c6\u7528Prometheus\u30b5\u30fc\u30d0\u30fc"),(0,o.kt)("li",{parentName:"ul"},"\u500b\u3005\u306eHydra\u30ce\u30fc\u30c9\u3068\u5bfe\u8a71\u3059\u308b\u305f\u3081\u306e\u30a2\u30c9\u30db\u30c3\u30af\u7aef\u672b\u30e6\u30fc\u30b6\u30fc\u30a4\u30f3\u30bf\u30fc\u30d5\u30a7\u30a4\u30b9\u30af\u30e9\u30a4\u30a2\u30f3\u30c8")),(0,o.kt)("div",{className:"admonition admonition-caution alert alert--warning"},(0,o.kt)("div",{parentName:"div",className:"admonition-heading"},(0,o.kt)("h5",{parentName:"div"},(0,o.kt)("span",{parentName:"h5",className:"admonition-icon"},(0,o.kt)("svg",{parentName:"span",xmlns:"http://www.w3.org/2000/svg",width:"16",height:"16",viewBox:"0 0 16 16"},(0,o.kt)("path",{parentName:"svg",fillRule:"evenodd",d:"M8.893 1.5c-.183-.31-.52-.5-.887-.5s-.703.19-.886.5L.138 13.499a.98.98 0 0 0 0 1.001c.193.31.53.501.886.501h13.964c.367 0 .704-.19.877-.5a1.03 1.03 0 0 0 .01-1.002L8.893 1.5zm.133 11.497H6.987v-2.003h2.039v2.003zm0-3.004H6.987V5.987h2.039v4.006z"}))),"Caution!")),(0,o.kt)("div",{parentName:"div",className:"admonition-content"},(0,o.kt)("p",{parentName:"div"},"genesis\u30d6\u30ed\u30c3\u30af\u304b\u3089\u59cb\u307e\u308b\u30a2\u30c9\u30db\u30c3\u30af\u306a\u30d7\u30e9\u30a4\u30d9\u30fc\u30c8\u958b\u767a\u30cd\u30c3\u30c8\u3092\u4f7f\u7528\u3059\u308b\u305f\u3081\u3001\u958b\u767a\u30cd\u30c3\u30c8\u306e\u8a2d\u5b9a\u304c\u6700\u65b0\u3067\u3042\u308b\u3053\u3068\u3092\u90fd\u5ea6\u78ba\u8a8d\u3059\u308b\u5fc5\u8981\u304c\u3042\u308a\u307e\u3059\u3002 Cardano \u30ce\u30fc\u30c9\u304b\u3089 ",(0,o.kt)("inlineCode",{parentName:"p"},"LedgerNoView")," \u30a8\u30e9\u30fc\u304c\u767a\u751f\u3057\u305f\u5834\u5408\u3001\u958b\u59cb\u6642\u523b\u304c\u904e\u53bb\u306b\u306a\u3063\u3066\u3044\u308b\u305f\u3081\u3001 ",(0,o.kt)("inlineCode",{parentName:"p"},"prepare-devnet.sh")," \u30b9\u30af\u30ea\u30d7\u30c8\u3092\u4f7f\u3063\u3066\u66f4\u65b0\u3059\u308b\u5fc5\u8981\u304c\u3042\u308a\u307e\u3059\u3002"))),(0,o.kt)("h2",{id:"\u30cd\u30c3\u30c8\u30ef\u30fc\u30af\u306e\u8a2d\u5b9a"},"\u30cd\u30c3\u30c8\u30ef\u30fc\u30af\u306e\u8a2d\u5b9a"),(0,o.kt)("p",null,"\u3053\u306e\u9805\u3067\u306f ",(0,o.kt)("a",{parentName:"p",href:"https://www.docker.com/get-started"},"Docker")," \u3068 ",(0,o.kt)("a",{parentName:"p",href:"https://www.docker.com/get-started"},"compose")," \u3092\u4f7f\u3063\u3066\u30c7\u30e2\u3092\u5b9f\u884c\u3057\u307e\u3059\u3002\u9ad8\u5ea6\u306a\u65b9\u6cd5\u3067\u5b9f\u884c\u3057\u305f\u3044\u5834\u5408\u306f ",(0,o.kt)("a",{parentName:"p",href:"/docs/getting-started/demo/without-docker"},"Docker\u3092\u4f7f\u7528\u3057\u306a\u3044\u30c7\u30e2\u306e\u5b9f\u884c")," \u3078\u79fb\u52d5\u3057\u3066\u304f\u3060\u3055\u3044\u3002"),(0,o.kt)("div",{className:"admonition admonition-info alert alert--info"},(0,o.kt)("div",{parentName:"div",className:"admonition-heading"},(0,o.kt)("h5",{parentName:"div"},(0,o.kt)("span",{parentName:"h5",className:"admonition-icon"},(0,o.kt)("svg",{parentName:"span",xmlns:"http://www.w3.org/2000/svg",width:"14",height:"16",viewBox:"0 0 14 16"},(0,o.kt)("path",{parentName:"svg",fillRule:"evenodd",d:"M7 2.3c3.14 0 5.7 2.56 5.7 5.7s-2.56 5.7-5.7 5.7A5.71 5.71 0 0 1 1.3 8c0-3.14 2.56-5.7 5.7-5.7zM7 1C3.14 1 0 4.14 0 8s3.14 7 7 7 7-3.14 7-7-3.14-7-7-7zm1 3H6v5h2V4zm0 6H6v2h2v-2z"}))),"Context")),(0,o.kt)("div",{parentName:"div",className:"admonition-content"},(0,o.kt)("p",{parentName:"div"},"\u4ee5\u4e0b\u306e\u30b3\u30de\u30f3\u30c9\u306f\u3059\u3079\u3066\u3001\u30d7\u30ed\u30b8\u30a7\u30af\u30c8\u30ea\u30dd\u30b8\u30c8\u30ea\u306e ",(0,o.kt)("inlineCode",{parentName:"p"},"demo")," \u30d5\u30a9\u30eb\u30c0\u304b\u3089\u5b9f\u884c\u3055\u308c\u308b\u3082\u306e\u3068\u3057\u3066\u66f8\u304b\u308c\u3066\u3044\u307e\u3059\u3002\u3057\u305f\u304c\u3063\u3066\u3001\u4f55\u304b\u3092\u3059\u308b\u524d\u306b\u30ea\u30dd\u30b8\u30c8\u30ea\u3092\u30af\u30ed\u30fc\u30f3\u3057\u3066 ",(0,o.kt)("inlineCode",{parentName:"p"},"cd demo")," \u3092\u5b9f\u884c\u3057\u3066\u8a72\u5f53\u30c7\u30a3\u30ec\u30af\u30c8\u30ea\u306b\u30dd\u30a4\u30f3\u30bf\u30fc\u3092\u5408\u308f\u305b\u3066\u304f\u3060\u3055\u3044\u3002"))),(0,o.kt)("div",{className:"admonition admonition-warning alert alert--danger"},(0,o.kt)("div",{parentName:"div",className:"admonition-heading"},(0,o.kt)("h5",{parentName:"div"},(0,o.kt)("span",{parentName:"h5",className:"admonition-icon"},(0,o.kt)("svg",{parentName:"span",xmlns:"http://www.w3.org/2000/svg",width:"12",height:"16",viewBox:"0 0 12 16"},(0,o.kt)("path",{parentName:"svg",fillRule:"evenodd",d:"M5.05.31c.81 2.17.41 3.38-.52 4.31C3.55 5.67 1.98 6.45.9 7.98c-1.45 2.05-1.7 6.53 3.53 7.7-2.2-1.16-2.67-4.52-.3-6.61-.61 2.03.53 3.33 1.94 2.86 1.39-.47 2.3.53 2.27 1.67-.02.78-.31 1.44-1.13 1.81 3.42-.59 4.78-3.42 4.78-5.56 0-2.84-2.53-3.22-1.25-5.61-1.52.13-2.03 1.13-1.89 2.75.09 1.08-1.02 1.8-1.86 1.33-.67-.41-.66-1.19-.06-1.78C8.18 5.31 8.68 2.45 5.05.32L5.03.3l.02.01z"}))),"OS Compatibility")),(0,o.kt)("div",{parentName:"div",className:"admonition-content"},(0,o.kt)("p",{parentName:"div"},"\u3053\u306e\u624b\u9806\u306f\u3001Linux\u74b0\u5883\uff08Ubuntu\u3001NixOS\uff09\u306e\u307f\u3067\u691c\u8a3c\u3057\u3066\u3044\u307e\u3059\u3002Windows\u3084Mac OS X\u306e\u5834\u5408\u306f\u3001",(0,o.kt)("a",{parentName:"p",href:"https://docs.docker.com/storage/volumes/"},"Volumes"),"\u3092\u4f7f\u7528\u3059\u308b\u3059\u308b\u5fc5\u8981\u304c\u3042\u308b\u304b\u3082\u3057\u308c\u307e\u305b\u3093\u3002"))),(0,o.kt)("p",null,"\u307e\u305a\u306f\u3001compose\u30d5\u30a1\u30a4\u30eb\u306b\u5b9a\u7fa9\u3055\u308c\u3066\u3044\u308b\u30b5\u30fc\u30d3\u30b9\u306b\u5fc5\u8981\u306a\u30a4\u30e1\u30fc\u30b8\u3092\u53d6\u5f97\u3057\u307e\u3057\u3087\u3046\u3002"),(0,o.kt)(d.Z,{mdxType:"TerminalWindow"},"docker-compose --profile tui pull"),(0,o.kt)("p",null,"\u3053\u3053\u304b\u3089\u3001",(0,o.kt)("inlineCode",{parentName:"p"},"./prepare-devnet.sh"),"\u30b9\u30af\u30ea\u30d7\u30c8\u3092\u5b9f\u884c\u3057\u3066\u3001\u958b\u767a\u7528\u30cd\u30c3\u30c8\u30ef\u30fc\u30af\u306e\u521d\u671f\u8a2d\u5b9a\u3092\u4f5c\u6210\u3059\u308b\u3053\u3068\u304c\u3067\u304d\u307e\u3059\u3002 \u3053\u308c\u306f\u3001Cardano\u30d6\u30ed\u30c3\u30af\u30c1\u30a7\u30fc\u30f3\u3092\u8d77\u52d5\u3059\u308b\u305f\u3081\u306b\u5fc5\u8981\u306a\u30b8\u30a7\u30cd\u30b7\u30b9\u30d5\u30a1\u30a4\u30eb\u3092\u4f5c\u6210\u3057\u307e\u3059\u3002\u306a\u304a\u3001\u4eca\u56de\u306e\u30c7\u30e2\u3067\u306f\u3001\u30b9\u30c6\u30fc\u30af\u30d7\u30fc\u30eb\u3092\u4e00\u5207\u5fc5\u8981\u3068\u3057\u306a\u3044\u30b7\u30f3\u30d7\u30eb\u306a\u69cb\u6210\u3092\u4f7f\u7528\u3057\u3066\u3044\u307e\u3059\u3002"),(0,o.kt)(d.Z,{mdxType:"TerminalWindow"},"./prepare-devnet.sh"),(0,o.kt)("p",null,"\u524d\u7f6e\u304d\u306f\u4ee5\u4e0a\u3067\u3059\u3002\u3053\u308c\u3067\u3001\u30cd\u30c3\u30c8\u30ef\u30fc\u30af\u3092\u7acb\u3061\u4e0a\u3052\u308b\u3053\u3068\u304c\u3067\u304d\u307e\u3059\u3002"),(0,o.kt)(d.Z,{mdxType:"TerminalWindow"},"docker-compose up -d"),(0,o.kt)("p",null,"\u4fbf\u5b9c\u4e0a\u3001\u4e0a\u8a18\u306e\u624b\u9806\u3092\u307e\u3068\u3081\u305f\u30b9\u30af\u30ea\u30d7\u30c8 ",(0,o.kt)("inlineCode",{parentName:"p"},"./run-docker.sh")," \u3092\u7528\u610f\u3057\u3066\u304a\u308a\u3001\u30b5\u30cb\u30c6\u30a3\u30fc\u30c1\u30a7\u30c3\u30af\u3092\u884c\u3046\u3053\u3068\u304c\u3067\u304d\u307e\u3059\u3002"),(0,o.kt)("h2",{id:"\u30cd\u30c3\u30c8\u30ef\u30fc\u30af\u306e\u69cb\u7bc9"},"\u30cd\u30c3\u30c8\u30ef\u30fc\u30af\u306e\u69cb\u7bc9"),(0,o.kt)("p",null,"\u73fe\u5728\u306e\u958b\u767a\u6bb5\u968e\u3067\u306f\u3001Hydra\u30ce\u30fc\u30c9\u306fHead\u30d7\u30ed\u30c8\u30b3\u30eb\u3092\u99c6\u52d5\u3059\u308b\u305f\u3081\u306b\u7279\u5225\u306b\u4f5c\u3089\u308c\u305fUTXO\u306e\u30bb\u30c3\u30c8\uff08\u300c\u71c3\u6599\u300d\uff09\u3068\u3001Head\u306b\u30b3\u30df\u30c3\u30c8\u3059\u308b\u305f\u3081\u306eUTXO\u304c\u5fc5\u8981\u3067\u3059\u3002\n\u540c\u68b1\u30b9\u30af\u30ea\u30d7\u30c8\u306e ",(0,o.kt)("inlineCode",{parentName:"p"},"./seed-devnet.sh")," \u306f\u3001\u3059\u3067\u306b\u5b9f\u884c\u4e2d\u306e ",(0,o.kt)("inlineCode",{parentName:"p"},"cardano-node")," \u30b3\u30f3\u30c6\u30ca\u306b\u3042\u308b ",(0,o.kt)("inlineCode",{parentName:"p"},"cardano-cli")," \u3092\u4f7f\u3063\u3066\u3001\u30a2\u30ea\u30b9\u3001\u30dc\u30d6\u3001\u30ad\u30e3\u30ed\u30eb\u306b\u30b3\u30df\u30c3\u30c8\u3059\u308b \u3044\u304f\u3064\u304b\u306eUTXO \u30a8\u30f3\u30c8\u30ea\u3068\u71c3\u6599\u306e UTXO \u3092\u6e21\u3057\u307e\u3059\u3002"),(0,o.kt)("div",{className:"admonition admonition-info alert alert--info"},(0,o.kt)("div",{parentName:"div",className:"admonition-heading"},(0,o.kt)("h5",{parentName:"div"},(0,o.kt)("span",{parentName:"h5",className:"admonition-icon"},(0,o.kt)("svg",{parentName:"span",xmlns:"http://www.w3.org/2000/svg",width:"14",height:"16",viewBox:"0 0 14 16"},(0,o.kt)("path",{parentName:"svg",fillRule:"evenodd",d:"M7 2.3c3.14 0 5.7 2.56 5.7 5.7s-2.56 5.7-5.7 5.7A5.71 5.71 0 0 1 1.3 8c0-3.14 2.56-5.7 5.7-5.7zM7 1C3.14 1 0 4.14 0 8s3.14 7 7 7 7-3.14 7-7-3.14-7-7-7zm1 3H6v5h2V4zm0 6H6v2h2v-2z"}))),"info")),(0,o.kt)("div",{parentName:"div",className:"admonition-content"},(0,o.kt)("p",{parentName:"div"},"\u3053\u308c\u3089\u306e\u30c8\u30e9\u30f3\u30b6\u30af\u30b7\u30e7\u30f3\u306f\u7279\u5225\u306a\u3082\u306e\u3067\u306f\u306a\u3044\u306e\u3067\u3001\u4ed6\u306e\u3069\u306eCardano\u30af\u30e9\u30a4\u30a2\u30f3\u30c8\u3067\u3082\u4f5c\u6210\u3067\u304d\u307e\u3059\u3002\u305f\u3060\u3057\u3001\u4ee5\u4e0b\u306e\u3088\u3046\u306a\u7279\u5fb4\u304c\u5fc5\u8981\u3067\u3059\u3002\nhydra-node\u5b9f\u884c\u30d5\u30a1\u30a4\u30eb\u306e\u5f15\u6570 ",(0,o.kt)("inlineCode",{parentName:"p"},"--cardano-signing-key")," \u3067\u5b9a\u7fa9\u3055\u308c\u305f\u3001Hydra Node\u306e\u5185\u90e8\u30a6\u30a9\u30ec\u30c3\u30c8\u3067\u4f7f\u7528\u3055\u308c\u308b\u30ad\u30fc\u306b\u30b3\u30df\u30c3\u30c8\u3059\u308b\u305f\u3081\u306b\u51fa\u529b\u3092\u652f\u6255\u3046\u5fc5\u8981\u304c\u3042\u308a\u307e\u3059\u3002\nOutput\u306e1\u3064\u306f\u3001\u30c7\u30fc\u30bf\u30e0\u30cf\u30c3\u30b7\u30e5 ",(0,o.kt)("inlineCode",{parentName:"p"},"a654fb60d21c1fed48db2c320aa6df9737ec0204c0ba53b9b94a09fb40e757f3")," \u3092\u542b\u3080\u5fc5\u8981\u304c\u3042\u308a\u307e\u3059\u3002\u3053\u308c\u306f\u300c\u71c3\u6599\u300d\u30de\u30fc\u30ab\u30fc\u3067\u3042\u308b\u304b\u3089\u3067\u3059\u3002"))),(0,o.kt)("h2",{id:"\u30af\u30e9\u30a4\u30a2\u30f3\u30c8\u306e\u5b9f\u884c"},"\u30af\u30e9\u30a4\u30a2\u30f3\u30c8\u306e\u5b9f\u884c"),(0,o.kt)("p",null,"compose \u3092\u4f7f\u3046\u3068\u3001Hydra \u30ce\u30fc\u30c9\u3068\u5bfe\u8a71\u3059\u308b\u305f\u3081\u306e\u30c7\u30e2\u7528\u30bf\u30fc\u30df\u30ca\u30eb\u30d9\u30fc\u30b9\u30e6\u30fc\u30b6\u30fc\u30a4\u30f3\u30bf\u30fc\u30d5\u30a7\u30a4\u30b9 (\u5225\u540d ",(0,o.kt)("inlineCode",{parentName:"p"},"hydra-tui"),") \u3092\u8d77\u52d5\u3059\u308b\u3053\u3068\u304c\u3067\u304d\u307e\u3059\u3002 compose\u306e\u5b9a\u7fa9\u306b\u306f\u3001",(0,o.kt)("inlineCode",{parentName:"p"},"hydra-tui-1"),", ",(0,o.kt)("inlineCode",{parentName:"p"},"hydra-tui-2"),", ",(0,o.kt)("inlineCode",{parentName:"p"},"hydra-tui-3")," \u3068\u3044\u30463\u3064\u306eTUI\u30b5\u30fc\u30d3\u30b9\u304c\u3042\u3089\u304b\u3058\u3081\u8a2d\u5b9a\u3055\u308c\u3066\u3044\u307e\u3059\u3002\u30bf\u30fc\u30df\u30ca\u30eb\u3067\u6700\u521d\u306eHydra\u30ce\u30fc\u30c9\u306b\u63a5\u7d9a\u3059\u308b\u305f\u3081\u306b\u3001\u4ee5\u4e0b\u306e\u30b3\u30de\u30f3\u30c9\u3092\u5b9f\u884c\u3057\u307e\u3059\u3002"),(0,o.kt)(d.Z,{mdxType:"TerminalWindow"},"docker-compose --profile tui run hydra-tui-1"),(0,o.kt)("p",null,"\u3053\u308c\u306f\u3001\u6700\u521d\u306eHydra\u30ce\u30fc\u30c9\u306b\u5bfe\u5fdc\u3059\u308b\u7f72\u540d\u30ad\u30fc\u3092\u30ed\u30fc\u30c9\u3057\u305f\u672c\u683c\u7684\u306a\u30bf\u30fc\u30df\u30ca\u30eb\u30a4\u30f3\u30bf\u30d5\u30a7\u30fc\u30b9\u3092\u958b\u59cb\u3059\u308b\u3082\u306e\u3067\u3059\u3002\u4ed6\u306e\u7aef\u672b\u3067\u306f\u3001",(0,o.kt)("inlineCode",{parentName:"p"},"hydra-tui-2"),"\u3068",(0,o.kt)("inlineCode",{parentName:"p"},"hydra-tui-3"),"\u306e\u30b5\u30fc\u30d3\u30b9\u3092\u30bf\u30fc\u30b2\u30c3\u30c8\u3068\u3057\u3066\u3001\u540c\u69d8\u306e\u65b9\u6cd5\u3067\u4ed6\u306e\u30ce\u30fc\u30c9\u3092\u8d77\u52d5\u3059\u308b\u3053\u3068\u304c\u3067\u304d\u307e\u3059\u3002"))}u.isMDXComponent=!0}}]);