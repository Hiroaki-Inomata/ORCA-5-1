      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ************************************************************
      *    �����ǡ����ʴ��ܾ����         COPY (CPCDF001.INC)    *
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  COLDAT01-REC.
           03  COLDAT01-KEY.
      *        ���ŵ��أɣ�
               05  COLDAT01-HOSPID         PIC  X(24).
      *        ����ǯ��
               05  COLDAT01-SRYYM          PIC  9(06).
      *        ������ʬ
               05  COLDAT01-NYUGAIKBN      PIC  X(01).
      *        ���ԣɣ�
               05  COLDAT01-PTID           PIC  9(10).
      *        �쥻�ץȿ��Ų�
               05  COLDAT01-RECEKA         PIC  X(02).
      *        ���ݸ��ݸ��ֹ�
               05  COLDAT01-HKNNUM         PIC  X(03).
      *        ����ɣ�
               05  COLDAT01-KOHID          PIC  9(10).
      *        �ݸ����ֹ�
               05  COLDAT01-HKNJANUM       PIC  X(08).
      *        �ܿͲ�²��ʬ
               05  COLDAT01-HONKZKKBN      PIC  X(01).
      *        �����ʬ
               05  COLDAT01-HOJOKBN        PIC  X(01).
      *
      *        ���ݸ��ɣ�
           03  COLDAT01-HKNID              PIC  9(10).
      *
      *    ���ݸ�����
           03  COLDAT01-HKN-AREA.
               05  COLDAT01-HKN-TBL        OCCURS  5.
      *            ���ݸ����ݸ��ɣ�
                   07  COLDAT01-THKNID     PIC  9(10).
      *            Ŭ�ѳ���ǯ����
                   07  COLDAT01-TTEKSTYMD  PIC  X(08).
      *            Ŭ�ѽ�λǯ����
                   07  COLDAT01-TTEKEDYMD  PIC  X(08).
      *            ���ݸ��ݷ�³��ʬ
                   07  COLDAT01-TCONTKBN   PIC  X(01).
      *            ���ݸ��������ʬ
                   07  COLDAT01-THOJOKBN   PIC X(01).
      *            ���ݸ����ܿͲ�²��ʬ
                   07  COLDAT01-THONKZKKBN PIC  X(01).
      *        ���ݸ���ˡ���ֹ�
               05  COLDAT01-HKNHBTNUM      PIC  X(02).
      *        ���ݸ��ݸ�����̶�ʬ
               05  COLDAT01-HKNKOHSBTKBN   PIC X(01).
      *    ������
           03  COLDAT01-HKNJNISSU          PIC  9(02).
      *    �������
           03  COLDAT01-HKNTOTALTENSU      PIC  9(08).
      *    �������ܲ��
           03  COLDAT01-HKNSKJNISSU        PIC  9(02).
      *    ����������
           03  COLDAT01-HKNSKJRYOYOHI      PIC  9(08).
      *    ����������ɸ����ô��
           03  COLDAT01-HKNSKJFTN          PIC  9(08).
      *
      *    Ϸ���ݷ����
           03  COLDAT01-RJNHKN.
      *        Ϸ���ݸ����ݸ��ֹ�
               05  COLDAT01-RJNHKNNUM      PIC  X(03).
      *        Ϸ�ͻ�ʧ��ʬ
               05  COLDAT01-RJNPAYKBN      PIC  X(02).
      *        Ϸ���ݸ���ˡ���ֹ�
               05  COLDAT01-RJNHBTNUM      PIC  X(02).
      *        Ϸ��Ŭ�ѳ���ǯ����
               05  COLDAT01-RJNTEKSTYMD    PIC  X(08).
      *        Ϸ��Ŭ�ѽ�λǯ����
               05  COLDAT01-RJNTEKEDYMD    PIC  X(08).
      *
      *    �������
           03 COLDAT01-KOHI-AREA.
               05  COLDAT01-KOHI-TBL       OCCURS  10.
      *            ����ɣ�
                   07  COLDAT01-TKOHID     PIC  9(10).
      *            �����ݸ��ֹ�
                   07  COLDAT01-TKOHHKNNUM PIC  X(03).
      *            �����ʧ��ʬ
                   07  COLDAT01-TKOHPAYKBN PIC  X(02).
      *            �����ˡ���ֹ�
                   07  COLDAT01-TKOHHBTNUM PIC  X(02).
      *            ����ݥ쥻�ץ������ʬ
                   07  COLDAT01-TRECESKYKBN PIC  X(01).
      *            Ŭ�ѳ���ǯ����
                   07  COLDAT01-TKOHTEKSTYMD    PIC  X(08).
      *            Ŭ�ѽ�λǯ����
                   07  COLDAT01-TKOHTEKEDYMD    PIC  X(08).
      *            ������
                   07  COLDAT01-TKOHJNISSU      PIC  9(02).
      *            �������
                   07  COLDAT01-TKOHTOTALTENSU  PIC  9(08).
      *            �������ܲ��
                   07  COLDAT01-TKOHSKJNISSU    PIC  9(02).
      *            ����������
                   07  COLDAT01-TKOHSKJRYOYOHI  PIC  9(08).
      *            ����������ɸ����ô��
                   07  COLDAT01-TKOHSKJFTN      PIC  9(08).
      *
      *    ϫ�Ҿ���
           03  COLDAT01-HKN.
      *        ϫ�ҡ�ϫ���ݸ���ʬ
               05  COLDAT01-HKNKBN          PIC  X(01).
      *        ϫ�ҡݺҳ���ʬ
               05  COLDAT01-SAIGAIKBN       PIC  X(01).
      *
      *    ���Ծ���
           03  COLDAT01-PTINF-AREA.
      *        ����
               05  COLDAT01-SEX             PIC  X(01).
      *        ��ǯ����
               05  COLDAT01-BIRTHDAY        PIC  9(08).
      *        ǯ��
               05  COLDAT01-AGE             PIC  9(03).
      *        ��������ʬ�ʣ�����������
               05  COLDAT01-SYUGAKUKBN      PIC  9(01).
      *        �����͹���ֹ�
               05  COLDAT01-HOME-POST       PIC  X(07).
      *    �ݸ��ȹ礻
           03  COLDAT01-HKNCOMBIINF.
               05  COLDAT01-HKNCOMBIINF     OCCURS  10.
      *�ݸ��ȹ礻�ֹ�
                   07  COLDAT01-HKNCOMBI    PIC  9(04).
      *���ݸ��ֹ�
                   07  COLDAT01-HKNNUMHC    PIC  X(03).
      *�ȹ礻�����ݸ��ֹ����
                   07  COLDAT01-KOHNUMHCINF OCCURS  4.
      *�ȹ礻�����ݸ��ֹ�
                       09  COLDAT01-KOHNUMHC
                                            PIC  X(03).
      *�ȹ礻�����ݸ��ɣ�
                       09  COLDAT01-KOHIDHC PIC  9(10).
      *    ���Ųʾ���
           03  COLDAT01-SRYKAINF.
               05  COLDAT01-SRYKA           PIC  X(02)  OCCURS  20.
      *    ����ǯ����
           03  COLDAT01-NYUINYMD            PIC  X(08).
      *    �ౡǯ����
           03  COLDAT01-TAIINYMD            PIC  X(08).
      *    ������ʧ����
           03  COLDAT01-TEISYUTUSAKI        PIC  9(01).
      *    �쥻�ץȼ���
           03  COLDAT01-RECESYUBETU         PIC  9(04).
      *
      *    ̤������ʬ
           03  COLDAT01-TEISEIKBN           PIC  X(01).
