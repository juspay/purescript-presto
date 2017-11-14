import React, { Component } from 'react';
import styles from './App.css';

class Header extends Component {
  render() {
    return (
      <h1 className={styles.header}>{this.props.name}</h1>
    );
  }
}


class Operator extends Component {
  render() {
    return (
      <div className = {styles.operatorList} onClick = {()=>{this.props.callBack(this.props.operatorName)}}>
        <div className = {styles.operator}>
          <img className = {styles.image} 
            src={require("../dist/"+(this.props.operatorName).toLowerCase()+".png")} 
            alt = {require("../dist/logo.svg")}/>
        </div>
        <div className={styles.operatorName}>{this.props.operatorName}</div>
      </div>
    );
  }
}


class ChooseOperator extends Component {
  constructor(state) {
    super(state);
    this.state = state
    this.onOperatorSelected = this.onOperatorSelected.bind(this);
  }

  onOperatorSelected(operatorName) {
    window.__runDuiCallback(JSON.stringify({tag:"OperatorSelected",contents:operatorName}))
  }

  render() {
    var operators = this.state.operators;
    var operatorsList = operators.map(
      (operator)=> <Operator operatorName={operator} callBack = {this.onOperatorSelected}/>
      );
    return (
      <div className = {styles.background}>
        <div className = {styles.chooseOperator}>
          <Header name="Choose Operator"/>
          {operatorsList}
        </div>
      </div>
    );
  }
}

export default ChooseOperator;
