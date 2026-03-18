(func $_mul (param $l v128) (param $r v128) (result v128)
	;; TODO
	unreachable
)

(func $re (param $v v128) (result v128)
	local.get $v
	v128.const f64x2 1.0 0.0
	f64x2.mul
)

(func $im (param $v v128) (result v128)
	local.get $v
	v128.const f64x2 0.0 1.0
	f64x2.mul
)
