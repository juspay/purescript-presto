import React, { Component } from 'react';
import styles from './App.css';

class Header extends Component {
  render() {
    return (
      <h3 className={styles.header}>{this.props.name}</h3>
    );
  }
}




class AskMobileNumber extends Component {
  constructor(state) {
    super(state);
    this.onMobileNumberEntered = this.onMobileNumberEntered.bind(this);
    this.onBackClick = this.onBackClick.bind(this);
  }

  onBackClick (){
    window.__runDuiCallback(JSON.stringify({tag:"AskMobileNumberScreenAbort"}))
  }

  onMobileNumberEntered() {
    if((document.getElementsByName('mobileNumber')[0].value).length == 10){
      window.__runDuiCallback(JSON.stringify({
        tag:"SubmitMobileNumber",
        contents:document.getElementsByName('mobileNumber')[0].value
      }))
    }
  }

  render() {
    return (
      <div className = {styles.background}>
        <div className = {styles.AskMobileNumber}>
          <div className={styles.headerWithArrow}>
            <div>
              <img onClick = {this.onBackClick} 
                className = {styles.backImage} 
                src={require("../dist/icon1.png")} 
                alt = {require("../dist/logo.svg")} 
                align = "left"/>
              <Header name="Mobile Number"/>
            </div>
          </div>
          <div className = {styles.body}>
            <h4>Enter your Mobile Number</h4>
            <input type="tel" name = "mobileNumber" length = "10" placeholder="+91"/>
          </div>
          <button onClick = {this.onMobileNumberEntered}
            className = {styles.button} 
            type="button">
            Next
          </button>
        </div>
      </div>
    );
  }
}

export default AskMobileNumber;
