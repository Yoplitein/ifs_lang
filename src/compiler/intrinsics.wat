(func $_mul (param $l v128) (param $r v128) (result v128)
	(local $l_re f64)
	(local $l_im f64)
	(local $r_re f64)
	(local $r_im f64)
	(local $ret_re f64)
	(local $ret_im f64)

	local.get $l
	f64x2.extract_lane 0
	local.set $l_re
	local.get $l
	f64x2.extract_lane 1
	local.set $l_im

	local.get $r
	f64x2.extract_lane 0
	local.set $r_re
	local.get $r
	f64x2.extract_lane 1
	local.set $r_im

	local.get $l_re
	local.get $r_re
	f64.mul
	local.get $l_im
	local.get $r_im
	f64.mul
	f64.sub
	local.set $ret_re

	local.get $l_re
	local.get $r_im
	f64.mul
	local.get $l_im
	local.get $r_re
	f64.mul
	f64.add
	local.set $ret_im

	v128.const f64x2 0.0 0.0
	local.get $ret_re
	f64x2.replace_lane 0
	local.get $ret_im
	f64x2.replace_lane 1
)

(func $_div (param $l v128) (param $r v128) (result v128)
	(local $l_re f64)
	(local $l_im f64)
	(local $r_re f64)
	(local $r_im f64)
	(local $denom f64)
	(local $ret_re f64)
	(local $ret_im f64)

	local.get $l
	f64x2.extract_lane 0
	local.set $l_re
	local.get $l
	f64x2.extract_lane 1
	local.set $l_im

	local.get $r
	f64x2.extract_lane 0
	local.set $r_re
	local.get $r
	f64x2.extract_lane 1
	local.set $r_im

	local.get $r_re
	local.get $r_re
	f64.mul
	local.get $r_im
	local.get $r_im
	f64.mul
	f64.add
	local.set $denom

	local.get $l_re
	local.get $r_re
	f64.mul
	local.get $l_im
	local.get $r_im
	f64.mul
	f64.add
	local.get $denom
	f64.div
	local.set $ret_re

	local.get $l_im
	local.get $r_re
	f64.mul
	local.get $l_re
	local.get $r_im
	f64.mul
	f64.sub
	local.get $denom
	f64.div
	local.set $ret_im

	v128.const f64x2 0.0 0.0
	local.get $ret_re
	f64x2.replace_lane 0
	local.get $ret_im
	f64x2.replace_lane 1
)

(func $_pow (param $l v128) (param $r v128) (result v128)
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
