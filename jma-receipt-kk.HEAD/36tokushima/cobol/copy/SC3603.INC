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
        01  SC03.
      *     ɽ��ǯ��
            03  SC03-PRTYM      PIC X(16).
      *     ����ǯ����
            03  SC03-SEIYMD     PIC X(22).
      *     ���ŵ��إ�����
            03  SC03-HOSPCDN    PIC X(20).
      *     ͹���ֹ�
            03  SC03-POST       PIC X(08).
      *     ���ŵ��ؽ���
            03  SC03-HOSPADRS   PIC X(100).
      *     ���ŵ���̾��
            03  SC03-HOSPNAME   PIC X(100).
      *     �����ֹ�
            03  SC03-TEL        PIC X(15).
      *     ���߼�̾
            03  SC03-KAISETU    PIC X(40).
      *     ������(���̹��ݡ��࿦���ݡ�����)
            03  SC03-GKENSU     PIC X(11)   OCCURS  3 TIMES.
      *     ����ơ��֥�
            03  SC03-SEIKYU-TBL         OCCURS  20 TIMES.
      *     ��Į¼�ֹ�
                05  SC03-CITYNUM    PIC X(06).
      *     ���ճ��
                05  SC03-KYURATE    PIC X(04).
      *     Ĺ��
                05  SC03-KOUGK      PIC X(02).
      *     �����ơ��֥�
                05  SC03-NYUGAI-TBL         OCCURS  2 TIMES.
      *     ���
                    07  SC03-KENSU      PIC X(05).
      *     ����
                    07  SC03-NISSU      PIC X(05).
      *     ����
                    07  SC03-TENSU      PIC X(09).
      *     ¾ˡ
                    07  SC03-TAHO       PIC X(09).
      *     ���ް�����ô��
                05  SC03-YKZFTN     PIC X(09).
