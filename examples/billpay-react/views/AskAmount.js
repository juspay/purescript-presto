import React, { Component } from 'react';
import styles from './App.css';

class Header extends Component {
  render() {
    return (
      <div className={styles.operator}>
        <h1 className={styles.header}>{this.props.name}</h1>
      </div>
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
            <div className = {styles.operator}>
              <img className = {styles.backImage} 
                onClick = {this.onBackClick} 
                src={require("../dist/icon1.png")} 
                alt = {require("../dist/logo.svg")} 
                align = "left"/>
            </div>
            <Header name="Enter Amount"/>
          </div>
          <div className = {styles.body}>
            <h4>Enter your Mobile Number</h4>
            <div className={styles.rows}>
              <div className = {styles.operator}>
                <img className = {styles.image} 
                  src={require("../dist/rupeeblack0.png")} 
                  alt = {require("../dist/logo.svg")}/>
              </div>
              <input type="tel" name = "amount" maxLength = "10" />
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
