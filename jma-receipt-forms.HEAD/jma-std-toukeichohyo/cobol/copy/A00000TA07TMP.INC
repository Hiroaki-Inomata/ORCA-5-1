      ******************************************************************
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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ******************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �Ģɼ
      *  ����ݡ��ͥ��̾  : ϫ�Ҽ����ո�̳�ҳ�����ɽ
      *  ������            : 
      *  Maj/Min/Rev  ������  ����       ����
      *   01.00.01    ���    12/11/30   ��������
      *   01.00.02    ���    16/07/13   �������Ԥ����פ���ʤ��Զ�����
      *******************************************************************
        01  TA07TMP.
            03  TA07TMP-KEY.
              05  TA07TMP-HKNKBN-NUM  PIC X(1).
              05  TA07TMP-NYUGAI      PIC 9(1).
              05  TA07TMP-PTNUM       PIC X(20).
              05  TA07TMP-HKNCOMBI    PIC 9(4).
            03  TA07TMP-HKNNUM      PIC 9(3).
            03  TA07TMP-PTNUM10     PIC X(10).
            03  TA07TMP-PTNUM20     PIC X(20).
            03  TA07TMP-NAME24      PIC X(24).
            03  TA07TMP-NAME48      PIC X(48).
            03  TA07TMP-HKNKBN      PIC X(12).
            03  TA07TMP-RHNUM       PIC 9(15).
            03  TA07TMP-SGIKBN      PIC X(16).
            03  TA07TMP-SKSBETU     PIC X(8).
            03  TA07TMP-SKYKBN      PIC X(24).
            03  TA07TMP-TENKI       PIC X(4).
            03  TA07TMP-SYMD        PIC X(9).
            03  TA07TMP-RYMDST      PIC X(9).
            03  TA07TMP-RYMDED      PIC X(9).
            03  TA07TMP-SKYTENSU    PIC 9(9).
            03  TA07TMP-SKYMONEY    PIC 9(11).