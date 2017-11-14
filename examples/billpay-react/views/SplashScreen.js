import React, { Component } from 'react';
import styles from './App.css';


class SplashScreen extends Component {
  constructor(state){
    super(state);
    setTimeout(function() { 
      window.__runDuiCallback(JSON.stringify({tag:"SplashScreenRendered"}))
    }.bind(this), 1500);
  }
  render() {
    return (
      <div className={styles.background}>
        <div className={styles.App}>
          <header>
            <h1 className={styles.AppTitle}>Bill Pay</h1>
          </header>
          <p className={styles.AppIntro}>
            Make recharge simple
          </p>
        </div>
      </div>
    );
  }
}

export default SplashScreen;
