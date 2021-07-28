#!/bin/bash

PLAYER=$(playerctl -al | head -n1 | awk '{print $1;}')
if [[ $PLAYER = 'spotify'    ]]; then
    echo ""
elif [[ $PLAYER = chrom*    ]]; then
    echo ""
else
    echo ""
fi

