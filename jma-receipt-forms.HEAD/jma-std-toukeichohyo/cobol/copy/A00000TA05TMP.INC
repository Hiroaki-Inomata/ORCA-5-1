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
      *******************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �Ģɼ
      *  ����ݡ��ͥ��̾  : ������̾�����Ź԰ٴ��԰���
      *  ������            : 
      *  Maj/Min/Rev  ������  ����        ����
      *   01.00.00    ����    2011/03/07  ��������
      *****************************************************************
       01  TA05TMP-REC.
           03  TA05TMP-KEY.
      *    ����ID
             05  TA05TMP-PTID              PIC X(10).
      *    ���š���̾����TBL
           03  TA05TMP-SRY-TBL             OCCURS 6.
      *    ���Ź԰�FLG������(1��6�����ϥѥ���б�)
             05  TA05TMP-SRYFLG            PIC 9(01).
      *    ��̾�����Ź԰�̾������(1��6�����ϥѥ���б�)
             05  TA05TMP-SRYNAME           PIC X(44).
      *    ��̾������
             05  TA05TMP-BYOSTYMD          PIC X(08).
      *    ž����
             05  TA05TMP-TENKIYMD          PIC X(08).
      *    ž��
             05  TA05TMP-TENKI             PIC X(04).
      *    �ǽ��象��
             05  TA05TMP-LASTYMD           PIC 9(08).