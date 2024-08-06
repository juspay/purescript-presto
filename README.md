Write apps like equations!

Code as simple and precise as Mathematical Expressions. Presto is written in PureScript harnessing the powers of Functional Programming & Category Theory. What you get is scalable, performant code with elegant abstractions. Our attempt with Presto is to really simplify the app development process.

[Talk on Presto @FunctionalConf](https://www.youtube.com/watch?v=HLEwYghBjo8)
<br/>
[Slides](https://speakerdeck.com/vimalkumar/presto-at-functional-conf-2017)

## Quickstart

You can get your hands dirty by building a mobile recharge app using Presto

```
git clone https://github.com/juspay/purescript-presto.git
cd purescript-presto/examples/billpay-react
npm i
bower i
npm start
```

Open http://localhost:8080/dist/ in your browser.

## Code Snippet

```
billPayFlow :: Flow BillPayFailure StatusScreenAction
billPayFlow = do
  _            <- UI.splashScreen
  operators    <- Remote.fetchOperators
  operator     <- UI.chooseOperator operators
  mobileNumber <- UI.askMobileNumber
  amount       <- UI.askAmount
  result       <- Remote.payBill mobileNumber amount operator
  UI.billPayStatus mobileNumber amount result
```

See [examples](https://github.com/juspay/purescript-presto/tree/master/examples/) directory for more samples.

## Examples

You can try out the below examples.

- [Counter Vanilla](https://github.com/juspay/purescript-presto/tree/master/examples/counter-html)
- [Bill Pay App using React](https://github.com/juspay/purescript-presto/tree/master/examples/billpay-react)
- [Bill Pay App using Presto-UI](https://github.com/juspay/purescript-presto/tree/master/examples/billpay-presto-ui)

## Learning Presto

[Presto Guide](https://juspay.gitbooks.io/presto-guide/content/) makes it easy to get started and start building great apps very quickly. We are continually improving Presto's documentation and welcome suggestions of topics we could explain in greater detail. Please send an email to presto@juspay.in with your requests.

## Add Presto to your existing project

```
bower i purescript-presto
```

## Apps in Production

[**_BHIM_**](https://play.google.com/store/apps/details?id=in.org.npci.upiapp&hl=en) - Payments App, 17 Million Total Users
<br>[**_JuspaySafe_**](https://juspay.in/juspay-safe) - Payments Browser, 800 Million Txns Processed
<br>[**_JuspayFuel_**](https://play.google.com/store/apps/details?id=in.juspay.euler.pregel&hl=en) - Payments Solution for Gas Stations, 1000s of Outlets

## Community

Get updates on improvements to Presto and chat with the project maintainers and community members.

Join a discussion or start one at our [forum](http://forum.juspayuniversity.in) or [gitter channel](https://gitter.im/Purescript-Presto/Lobby#).

## Contributing

See the [CONTRIBUTING.md](CONTRIBUTING.md) file for details.
