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
        01  SC11.
      *���ŵ��إ�����
            03  SC11-HOSPCD                           PIC X(14).
      *���ŵ��ؽ���
            03  SC11-ADRS                             PIC X(100).
      *���ŵ���̾
            03  SC11-HOSPNAME                         PIC X(100).
      *���߼�̾
            03  SC11-KAISETU                          PIC X(60).
      *����ǯ��
            03  SC11-SRYYM                            PIC X(16).
      *����ǯ����
            03  SC11-SEIYMD.
              05  SC11-SEIYY                          PIC X(04).
              05  SC11-SEIMM                          PIC X(04).
              05  SC11-SEIDD                          PIC X(04).
      *����ơ��֥�
            03  SC11-SEIKYU-TBL                       OCCURS  5 TIMES.
      *���ݸ���ʬ
              05  SC11-SHUHKN-TBL                     OCCURS  2 TIMES.
      *������ʬ
                07  SC11-NYUGAI-TBL                   OCCURS  2 TIMES.
      *���
                  09  SC11-KENSU                      PIC X(05).
      *����
                  09  SC11-TENSU                      PIC X(09).
      *��ͽ��������ô����
                  09  SC11-KOHTENSU                   PIC X(07).
      *���ް�����ô��
                  09  SC11-YKZFTN                     PIC X(07).
      *����Ի�ʧ��
                  09  SC11-SIHARAIGK                  PIC X(07).
      *����������ɸ����ô��
                  09  SC11-SFTNGK                     PIC X(09).
      *���̰����������
                  09  SC11-SEIKYUGK                   PIC X(09).
      *��ס����
            03  SC11-GKENSU                         PIC X(05).
      *��ס����̰����������
            03  SC11-GSEIKYUGK                      PIC X(10).
