//
//  auCloudViewController.m
//  auCloud
//
//  Created by Andrew Brunner on 7/14/12.
//  Copyright (c) 2012 Aurawin LLC. All rights reserved.
//

#import "auCloudViewController.h"

@interface auCloudViewController ()

@end

@implementation auCloudViewController
@synthesize auBrowser;

- (void)viewDidLoad
{
    [auBrowser loadRequest:[NSURLRequest requestWithURL:[NSURL URLWithString:@"http://aurawin.com"]]];
    [super viewDidLoad];
	
    // Do any additional setup after loading the view, typically from a nib.
}

- (void)viewDidUnload
{
    [self setAuBrowser:nil];
    [super viewDidUnload];
    // Release any retained subviews of the main view.
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone) {
        return (interfaceOrientation != UIInterfaceOrientationPortraitUpsideDown);
    } else {
        return YES;
    }
}

@end
