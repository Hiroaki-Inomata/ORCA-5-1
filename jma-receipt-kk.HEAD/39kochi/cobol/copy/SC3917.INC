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
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ���    2019/01/07  �������򥳥��Ⱥ��
      ************************************************************
        01  SC17.
      *ɽ��ǯ��
            03  SC17-PRTYM                       PIC X(16).
      *����ǯ����
            03  SC17-SEIYMD                      PIC X(22).
      *���ŵ��إ�����
            03  SC17-HOSPCDN                     PIC X(20).
      *���ŵ��ؽ���
            03  SC17-ADRS                        PIC X(100).
      *���ŵ���̾��
            03  SC17-HOSPNAME                    PIC X(100).
      *���߼�̾
            03  SC17-KAISETU                     PIC X(40).
      *��Į¼̾
            03  SC17-CITYNAME                    PIC X(26).
      *��ô���ֹ�
            03  SC17-FTNJANUMG.
              05  SC17-FTNJANUM
                                    PIC X(02)    OCCURS  8 TIMES.
      *������ֹ�
            03  SC17-JKYSNUMG.
              05  SC17-JKYSNUM
                                    PIC X(02)    OCCURS  7 TIMES.
      *�ݸ����ֹ�
            03  SC17-HKNJANUM                    PIC X(16).
      *���桦�ֹ�
            03  SC17-KIGO                        PIC X(20).
            03  SC17-BANGO                       PIC X(20).
      *���ʻ�̾
            03  SC17-KANANAME                    PIC X(50).
      *��̾
            03  SC17-NAME                        PIC X(30).
      *���ճ��
            03  SC17-KYURATE                     PIC X(02).
      *����
            03  SC17-SEXG                        OCCURS  2 TIMES.
              05  SC17-SEX                       PIC X(02).
      *��������
            03  SC17-SEIKYU-TBL                  OCCURS  2 TIMES.
      *������
              05  SC17-NISSU                     PIC X(04).
      *����
              05  SC17-TENSU                     PIC X(12).
      *���
              05  SC17-SEIKYUGK                  PIC X(12).
      *Ĺ��
            03  SC17-TYOKI1                      PIC X(02).
            03  SC17-TYOKI2                      PIC X(02).
      *����
            03  SC17-BIKO                        PIC X(16).
            03  SC17-BIKO2                       PIC X(16).
      *¾ˡˡ��
            03  SC17-TAHOUHBT                    PIC X(02).
      *¾ˡ��ô��
            03  SC17-TAHOUFTN                    PIC X(14).
      *���͡��������˷��봵����ô��
            03  SC17-BIKOTAHOU                   PIC X(28).
