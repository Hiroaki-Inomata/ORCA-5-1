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
        01  SC07.
      *����Ƚ��
            03  SC07-KOHKBN
                                       PIC X(02)    OCCURS  7 TIMES.
      *ͭ������
            03  SC07-YUKOSTYY                       PIC X(04).
            03  SC07-YUKOSTMM                       PIC X(04).
            03  SC07-YUKOSTDD                       PIC X(04).
            03  SC07-YUKOEDYY                       PIC X(04).
            03  SC07-YUKOEDMM                       PIC X(04).
            03  SC07-YUKOEDDD                       PIC X(04).
      *��Į¼������
            03  SC07-CITYNUM                        PIC X(12).
      *��Į¼̾
            03  SC07-CITYNAME                       PIC X(18).
      *��������
            03  SC07-SEIYMDG.
              05  SC07-SEIYY                        PIC X(04).
              05  SC07-SEIMM                        PIC X(04).
              05  SC07-SEIDD                        PIC X(04).
      *���ŵ��إ�����
            03  SC07-HOSPCD                         PIC X(14).
      *���ŵ��ؽ���
            03  SC07-ADRS                           PIC X(100).
      *���ŵ���̾
            03  SC07-HOSPNAME                       PIC X(100).
      *���߼�̾
            03  SC07-KAISETU                        PIC X(30).
      *����ǯ��
            03  SC07-SRYYMG.
              05  SC07-SRYYY                        PIC X(04).
              05  SC07-SRYMM                        PIC X(04).
      *������ʬ
            03  SC07-NYUGAIKBN
                                       PIC X(02)    OCCURS  2 TIMES.
      *������ֹ�
            03  SC07-JKYSKIGO                       PIC X(04).
            03  SC07-JKYSNUM                        PIC X(14).
      *���桦�ֹ�
            03  SC07-KIGO                           PIC X(18).
            03  SC07-NUM                            PIC X(20).
      *��̾
            03  SC07-NAME                           PIC X(24).
      *����
            03  SC07-SEX
                                       PIC X(02)    OCCURS  2 TIMES.
      *����
            03  SC07-GENGO
                                       PIC X(02)    OCCURS  4 TIMES.
      *����ǯ
            03  SC07-TANJOYY                        PIC X(02).
      *�ݸ�����
            03  SC07-HKNSYUBETU
                                       PIC X(02)    OCCURS  3 TIMES.
      *�ݸ����ֹ�
            03  SC07-HKNJANUM                       PIC X(16).
      *�ݸ���̾
            03  SC07-HKNJANAME                      PIC X(30).
      *���ճ��
            03  SC07-FTNRATE
                                       PIC X(02)    OCCURS  4 TIMES.
      *��������
            03  SC07-NNISSU                         PIC X(02).
      *������
            03  SC07-NYUINYY                        PIC X(04).
            03  SC07-NYUINMM                        PIC X(04).
            03  SC07-NYUINDD                        PIC X(04).
      *�ౡ��
            03  SC07-TAIINYY                        PIC X(04).
            03  SC07-TAIINMM                        PIC X(04).
            03  SC07-TAIINDD                        PIC X(04).
      *��������
            03  SC07-GNISSU                         PIC X(02).
      *������
            03  SC07-TENSU                          PIC X(16).
      *��ͽ����
            03  SC07-KESETEN                        PIC X(14).
      *���ް�����ô��
            03  SC07-YKZFTN                         PIC X(07).
      *����Ի�ʧ����
            03  SC07-SIHARAIGK                      PIC X(06).
      *����Ի�ʧ�����
            03  SC07-SIHARAIKAISU                   PIC X(02).
      *����Ի�ʧ������
            03  SC07-SIHARAINISSU1                  PIC X(02).
            03  SC07-SIHARAINISSU2                  PIC X(02).
      *��������
            03  SC07-SHOKUJINISSU                   PIC X(02).
      *ɸ����ô��
            03  SC07-SFTNGK                         PIC X(06).
      *�����
            03  SC07-SEIKYUGK                       PIC X(09).
      *��ʬ
            03  SC07-ETC-KBN
                                       PIC X(02)    OCCURS  10 TIMES.
