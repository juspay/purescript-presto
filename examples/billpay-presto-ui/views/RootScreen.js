var dom = require("presto-ui").doms;
var View = require("presto-ui").baseView;
var RelativeLayout = require("presto-ui").views.RelativeLayout;

class RootScreen extends View {
  constructor(props, children) {
    super(props, children);
    this.setIds(['root']);
  }

  render() {

    this.layout = (
      <RelativeLayout
        root="true"
        width="match_parent"
        height="match_parent"
        id = {this.idSet.root}/>
    );

    return this.layout.render();
  }
}

module.exports = RootScreen;
