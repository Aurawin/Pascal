//
//  ViewController.swift
//  AuVDM
//
//  Created by Andrew Brunner on 1/30/15.
//  Copyright (c) 2015 Aurawin LLC. All rights reserved.
//

import UIKit;

class ViewController: UIViewController,UIWebViewDelegate {
    var Loaded : Bool = false;
    
    func webView(webView: UIWebView!, didFailLoadWithError error: NSError!) {
        Loaded = false;
    }
    
    func webView(webView: UIWebView!, shouldStartLoadWithRequest request: NSURLRequest!, navigationType: UIWebViewNavigationType) -> Bool {
        if navigationType == UIWebViewNavigationType.LinkClicked {
            if (Loaded == true) {
              UIApplication.sharedApplication().openURL(request.URL!);
              return false;
            } else {
               return false;
            };
        };
        return true;
    }
    
    func webViewDidStartLoad(webView: UIWebView!) {
        Loaded = true;
    }
    
    func webViewDidFinishLoad(webView: UIWebView!) {
        Loaded = true;
    }
    
    
    @IBOutlet var Browser: UIWebView!
    override func viewDidLoad() {
        super.viewDidLoad();
      
        let Defaults : NSUserDefaults = NSUserDefaults.standardUserDefaults();
        if (Defaults.stringForKey("urlSite") == nil) {
            let sSite = "https://aurawin.com/" as NSString;
            Defaults.setValue(sSite, forKey: "urlSite");
            Defaults.synchronize();
        };
        let url = NSURL(string: Defaults.stringForKey("urlSite")!);
        let request = NSURLRequest(URL: url!);
        Browser.loadRequest(request);
   }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning();

    }
    
    override func prefersStatusBarHidden () -> Bool{
        return true;
    }
}

