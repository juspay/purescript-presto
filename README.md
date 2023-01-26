# <img src='./assets/presto-logo.png' height='60'>

Write apps like equations!

Code as simple and precise as Mathematical Expressions. Presto is written in PureScript harnessing the powers of Functional Programming & Category Theory. What you get is scalable, performant code with elegant abstractions. Our attempt with Presto is to really simplify the app development process.

[Talk on Presto @FunctionalConf](https://www.youtube.com/watch?v=HLEwYghBjo8)
<br/>
[Slides](https://speakerdeck.com/vimalkumar/presto-at-functional-conf-2017)


## Prerequisites

- [***npm***](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm)
- [***purescript***](https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md)

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

* [Counter Vanilla](https://github.com/juspay/purescript-presto/tree/master/examples/counter-html)
* [Bill Pay App using React](https://github.com/juspay/purescript-presto/tree/master/examples/billpay-react)
* [Bill Pay App using Presto-UI](https://github.com/juspay/purescript-presto/tree/master/examples/billpay-presto-ui)

## Learning Presto

[Presto Guide](https://juspay.gitbooks.io/presto-guide/content/) makes it easy to get started and start building great apps very quickly. We are continually improving Presto's documentation and welcome suggestions of topics we could explain in greater detail. Please send an email to presto@juspay.in with your requests.


## Add Presto to your existing project

```
bower i purescript-presto
```

## Apps in Production

[***BHIM***](https://play.google.com/store/apps/details?id=in.org.npci.upiapp&hl=en) - Payments App, 17 Million Total Users
<br>[***HyperSDK***](https://juspay.in/products#hypersdk) - A cross-platform, custom branded payment page solution. A single integration gives access to all and ever-changing payment methods.
<br>[***Yatri***](https://play.google.com/store/apps/details?id=net.openkochi.yatri&hl=en_IN&gl=US) - Yatri is a platform for planning your travel in Ernakulam. View the schedules for Kochi Metro or book taxi rides from Ernakulam to anywhere in Kerala, with local taxi operators.
<br>[***GemSahay***](https://play.google.com/store/apps/details?id=in.sahay.gem&hl=en_IN&gl=US) - The GeM Sahay mobile application enables you to apply for and avail short term financing offered by Banks and NBFCs such as Kotak Mahindra Bank, UGRO Capital, Tata Capital, 121 Finance on the Purchase Orders that you receive on the GeM portal. This program has participation from some of the top lenders in the country.
<br>***JuspayFuel*** - Payments Solution for Gas Stations, 1000s of Outlets


## Community

Get updates on improvements to Presto and chat with the project maintainers and community members.

Raise github issues as required and maintainers will reach out to you or drop an email to presto@juspay.in.
## Contributing

See the [CONTRIBUTING.md](CONTRIBUTING.md) file for details.



