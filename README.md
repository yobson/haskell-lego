Some fun with yampa and the spike prime!

The library is both simple, and slightly broken! So use with caution!

## Spikectl
This is a monadic wrapper that lets you control the spike prime!
It works pretty well... sometimes!

It almost never works over bluetooth, but works mostly over USB.
Sometimes, it gets in an error state on launch, but you just have to re-run your code (_should be fixed_)

## Robo
A simple yampa program using the spike library above!

We warned, setting a motor to a speed doesn't work with yampa alone!
This is because yampa calls it on every iteration. This means that the motor restarts
and never gets up to speed! Set by PWM

## Plans
- [X] initial spike library
- [X] yampa experiment
- [X] Fix startup of spike library
- [ ] Stablise spike library over bluetooth
- [ ] RPi BuildHAT Library (including firmware upload)
- [ ] Hackage documentation
- [ ] Upload to hackage!
- [ ] Yampa helpers
