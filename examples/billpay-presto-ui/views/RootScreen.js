var dom = require("@juspay/mystique").doms;
var View = require("@juspay/mystique").baseView;
var RelativeLayout = require("@juspay/mystique").views.RelativeLayout;

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
