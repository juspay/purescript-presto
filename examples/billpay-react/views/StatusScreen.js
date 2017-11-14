import React, { Component } from 'react';
import styles from './App.css';


class AskAmount extends Component {
constructor(state) {
    super(state);
    this.state = state;
    this.amount = this.state.state[1];
    this.mobileNumber = this.state.state[0];
  }
  render() {
    return (
      <div className = {styles.background}>
        <div className = {styles.status}>
          <img height = "100" 
            src={require("../dist/illustration2.png")} 
            alt = {require("../dist/logo.svg")}/>
          <h1>Success</h1>
          <p>You have successfully recharged {this.mobileNumber} with amount Rs {this.amount}.</p>
        </div>
      </div>
    );
  }
}

export default AskAmount;
