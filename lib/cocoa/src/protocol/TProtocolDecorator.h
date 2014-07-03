//
//  TProtocolDecorator.h
//  TaLiCaiCommunity
//
//  Created by guoyalun on 7/3/14.
//  Copyright (c) 2014 guoyalun. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "TProtocol.h"

@interface TProtocolDecorator : NSObject <TProtocol>
{
    id<TProtocol> concreteProtocol;
}
- (id)initWithProtocol:(id <TProtocol>)protocol;


@end
