program super_simple_gridgen
    use iric
    implicit none

    character(256) :: condfile
    integer(4) :: icount, istatus, fid, ier, j_chl, nx, ny, ni, nj, i, j
    real(8):: chl, width, dx, dy
    real(8), dimension(:, :), allocatable :: x, y, z

    !---------------------------------------------
    !  引数の確認→CGNSファイル名の取得
    !---------------------------------------------

    icount = nargs()
    if (icount == 2) then
        call getarg(1, condfile, istatus)
    else
        write (*, *) "Input File not specified."
        stop
    end if

    !---------------------------------------------
    !  CGNSファイルから格子生成条件の読込み
    !---------------------------------------------

    call cg_iric_open(trim(condfile), IRIC_MODE_MODIFY, fid, ier)
    if (ier /= 0) STOP "*** Open error of CGNS file ***"

    !---------------------------------------------
    !  格子生成条件を読み込む
    !---------------------------------------------

    call cg_iric_read_integer(fid, "j_chl", j_chl, ier)

    call cg_iric_read_integer(fid, "nx", nx, ier)
    call cg_iric_read_integer(fid, "ny", ny, ier)

    ! j_chl == 0 全長とセル数からセルのサイズを求める
    ! j_chl == 1 セルのサイズとセル数から全長を求める
    if (j_chl == 0) then
        call cg_iric_read_real(fid, "chl", chl, ier)
        dx = chl/float(nx)
    else
        call cg_iric_read_real(fid, "dx", dx, ier)
        chl = dx*float(nx)
    end if

    ! j_chl == 0 全長とセル数からセルのサイズを求める
    ! j_chl == 1 セルのサイズとセル数から全長を求める
    if (j_chl == 0) then
        call cg_iric_read_real(fid, "width", width, ier)
        dy = width/float(ny)
    else
        call cg_iric_read_real(fid, "dy", dy, ier)
        width = dy*float(ny)
    end if

    ! 全長、セルのサイズが0の時はエラーを返して終了する。
    if (dx == 0 .or. chl == 0 .or. dy == 0 .or. width == 0) then
        call cg_iric_write_errorcode(fid, 1, ier)
        call cg_iric_close(fid, ier)
        stop
    end if

    !---------------------------------------------
    !  格子点の数を求める
    !---------------------------------------------

    ni = nx + 1
    nj = ny + 1

    !---------------------------------------------
    !  変数のアロケート
    !---------------------------------------------

    allocate (x(1:ni, 1:nj), y(1:ni, 1:nj), z(1:ni, 1:nj))

    !---------------------------------------------
    !  変数の中身の初期化
    !---------------------------------------------

    x = 0.
    y = 0.
    z = 0.

    !---------------------------------------------
    !  各格子点の座標の計算(河床高=zは一律で0)
    !---------------------------------------------

    do i = 1, ni
        do j = 1, nj
            x(i, j) = dx*(i - 1)
            y(i, j) = dy*(j - 1)
        end do
    end do

    !---------------------------------------------
    !　格子データを出力する
    !---------------------------------------------

    call cg_iRIC_Write_Grid2d_Coords(fid, ni, nj, x, y, ier)
    call cg_iric_write_grid_real_node(fid, "Elevation", z, ier)

    !---------------------------------------------
    !    CGNSファイルを閉じる
    !---------------------------------------------
    call cg_iric_close(fid, ier)

end program super_simple_gridgen
