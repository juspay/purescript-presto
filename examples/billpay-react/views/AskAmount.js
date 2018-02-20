import React, { Component } from 'react';
import styles from './App.css';

class Header extends Component {
  render() {
    return (
      <h3 className={styles.header}>{this.props.name}</h3>
    );
  }
}




class AskAmount extends Component {
  constructor(state) {
    super(state);
    this.onAmountEntered = this.onAmountEntered.bind(this);
    this.onBackClick = this.onBackClick.bind(this);
  }

  onBackClick () {
    window.__runDuiCallback(JSON.stringify({tag:"AskAmountScreenAbort"}))
  }

  onAmountEntered() {
    if(Number(document.getElementsByName('amount')[0].value) > 0){
      window.__runDuiCallback(JSON.stringify({
        tag:"SubmitAmount",
        contents:Number(document.getElementsByName('amount')[0].value)
      }))
    }
  }

  render() {
    return (
      <div className = {styles.background}>
        <div className = {styles.AskMobileNumber}>
          <div className={styles.headerWithArrow}>
              <img className = {styles.backImage} 
                onClick = {this.onBackClick} 
                src={require("../dist/icon1.png")} 
                alt = {require("../dist/logo.svg")} 
                align = "left"/>
            <Header name="Enter Amount"/>
          </div>
          <div className = {styles.body}>
            <h4>Enter amount to be recharged</h4>
            <div className={styles.rows}>
              <input type="tel" name = "amount" maxLength = "10" placeholder="Rs. 100 /-" />
            </div>
          </div>
          <button className = {styles.button}
            onClick = {this.onAmountEntered} 
            type="button"> 
            Next
          </button>
        </div>
      </div>
    );
  }
}

export default AskAmount;
