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
        01  SC05.
      *ɽ��ǯ��
            03  SC05-PRTYM                    PIC X(16).
      *����ǯ����
            03  SC05-SEIYMD                   PIC X(22).
      *���ŵ��إ�����
            03  SC05-HOSPCDN                  PIC X(20).
      *���ŵ��ؽ���
            03  SC05-ADRS                     PIC X(100).
      *���ŵ���̾��
            03  SC05-HOSPNAME                 PIC X(100).
      *���߼�̾
            03  SC05-KAISETU                  PIC X(40).
      *��Į¼̾
            03  SC05-CITYNAME                 PIC X(26).
      *��ô���ֹ�
            03  SC05-FTNJANUMG.
              05  SC05-FTNJANUM
                                          PIC X(02)    OCCURS  8 TIMES.
      *������ֹ�
            03  SC05-JKYSNUMG.
              05  SC05-JKYSNUM
                                          PIC X(02)    OCCURS  7 TIMES.
      *�ݸ����ֹ�
            03  SC05-HKNJANUM                 PIC X(16).
      *���桦�ֹ�
            03  SC05-KIGO                     PIC X(20).
            03  SC05-BANGO                    PIC X(20).
      *���ʻ�̾
            03  SC05-KANANAME                 PIC X(50).
      *��̾
            03  SC05-NAME                     PIC X(30).
      *���ճ��
            03  SC05-KYURATE                  PIC X(02).
      *����
            03  SC05-SEXG                   OCCURS  2 TIMES.
              05  SC05-SEX                    PIC X(02).
      *��������
            03  SC05-SEIKYU-TBL             OCCURS  3 TIMES.
      *������
              05  SC05-NISSU                  PIC X(04).
      *����
              05  SC05-TENSU                  PIC X(12).
      *���
              05  SC05-SEIKYUGK               PIC X(12).
      *Ĺ��
            03  SC05-TYOKI1                   PIC X(02).
            03  SC05-TYOKI2                   PIC X(02).
      *����
            03  SC05-BIKO                     PIC X(16).
            03  SC05-BIKO2                    PIC X(16).
            03  SC05-BIKO3                    PIC X(16).
      *¾ˡˡ��
            03  SC05-TAHOUHBT                 PIC X(02).
      *¾ˡ��ô��
            03  SC05-TAHOUFTN                 PIC X(14).