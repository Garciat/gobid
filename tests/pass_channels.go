package tests

type Hello interface {
	hello()
}

type Example struct{}

func (Example) hello() {}

func pump[T U, U any](src <-chan T, dst chan<- U) {
	for v := range src {
		dst <- v
	}
}

func init() {
	pump(make(<-chan Example), make(chan<- Hello))
}
