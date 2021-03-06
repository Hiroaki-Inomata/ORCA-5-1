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
        01  SC09.
      *�����ʬ
            03  SC09-KOHKBN
                                        PIC X(02)    OCCURS  7 TIMES.
      *ͭ������
            03  SC09-YUKOSTYY                        PIC X(04).
            03  SC09-YUKOSTMM                        PIC X(04).
            03  SC09-YUKOSTDD                        PIC X(04).
            03  SC09-YUKOEDYY                        PIC X(04).
            03  SC09-YUKOEDMM                        PIC X(04).
            03  SC09-YUKOEDDD                        PIC X(04).
      *��Į¼������
            03  SC09-CITYNUM                         PIC X(12).
      *��Į¼̾
            03  SC09-CITYNAME                        PIC X(18).
      *��������
            03  SC09-SEIYMDG.
              05  SC09-SEIYY                         PIC X(04).
              05  SC09-SEIMM                         PIC X(04).
              05  SC09-SEIDD                         PIC X(04).
      *���ŵ��إ�����
            03  SC09-HOSPCD                          PIC X(14).
      *���ŵ��ؽ���
            03  SC09-ADRS                            PIC X(100).
      *���ŵ���̾
            03  SC09-HOSPNAME                        PIC X(100).
      *���߼�̾
            03  SC09-KAISETU                         PIC X(30).
      *����ǯ��
            03  SC09-SRYYMG.
              05  SC09-SRYYY                         PIC X(04).
              05  SC09-SRYMM                         PIC X(04).
      *������ʬ
            03  SC09-NYUGAIKBN
                                        PIC X(02)    OCCURS  2 TIMES.
      *������ֹ�
            03  SC09-JKYSKIGO                        PIC X(04).
            03  SC09-JKYSNUM                         PIC X(14).
      *���桦�ֹ�
            03  SC09-KIGO                            PIC X(12).
            03  SC09-NUM                             PIC X(20).
      *��̾
            03  SC09-NAME                            PIC X(24).
      *����
            03  SC09-SEX
                                        PIC X(02)    OCCURS  2 TIMES.
      *����
            03  SC09-GENGO
                                        PIC X(02)    OCCURS  4 TIMES.
      *����ǯ
            03  SC09-TANJOYY                         PIC X(02).
      *�ݸ�����
            03  SC09-HKNSYUBETU
                                        PIC X(02)    OCCURS  4 TIMES.
      *�ݸ����ֹ�
            03  SC09-HKNJANUM                        PIC X(16).
      *�ݸ���̾
            03  SC09-HKNJANAME                       PIC X(30).
      *���ճ��
            03  SC09-FTNRATE
                                        PIC X(02)    OCCURS  4 TIMES.
      *��������
            03  SC09-NNISSU                          PIC X(02).
      *������
            03  SC09-NYUINYY                         PIC X(04).
            03  SC09-NYUINMM                         PIC X(04).
            03  SC09-NYUINDD                         PIC X(04).
      *�ౡ��
            03  SC09-TAIINYY                         PIC X(04).
            03  SC09-TAIINMM                         PIC X(04).
            03  SC09-TAIINDD                         PIC X(04).
      *��������
            03  SC09-GNISSU                          PIC X(02).
      *������
            03  SC09-TENSU                           PIC X(16).
      *��ͽ����
            03  SC09-KESETEN                         PIC X(14).
      *���ް�����ô��
            03  SC09-YKZFTN                          PIC X(07).
      *����Ի�ʧ����
            03  SC09-SIHARAIGK                       PIC X(06).
      *����Ի�ʧ�����
            03  SC09-SIHARAIKAISU                    PIC X(02).
      *����Ի�ʧ������
            03  SC09-SIHARAINISSU1                   PIC X(02).
            03  SC09-SIHARAINISSU2                   PIC X(02).
      *��������
            03  SC09-SHOKUJINISSU                    PIC X(02).
      *ɸ����ô��
            03  SC09-SFTNGK                          PIC X(06).
      *�����
            03  SC09-SEIKYUGK                        PIC X(09).
      *��ʬ
            03  SC09-ETC-KBN
                                        PIC X(02)    OCCURS  10 TIMES.
