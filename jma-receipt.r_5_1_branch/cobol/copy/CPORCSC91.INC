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
      *----------------------------------------------------------*
      *    ���Ź԰ٻ���������å���   COPY (CPORCSC91.INC)
      *                              (ORCSC91 �ǻ��ѡ�
      *
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  02/09/02  ʻ�ѻ���ʬ��ɽ��
      * 04.07.00     NACL-¿��Ǽ  11/11/29  ��������ʬ�ɲ�
      ************************************************************
      *
       01  ORCSC91AREA.
      *����
      *    1:����ʬ   2:��ư�ɲ�ʬ 3�������å�ʬ
      *    9:�������� 8:��³�������֡ʷ�³��������������
           03  LNK-SC91-KBN            PIC X(01).
      *    �����ե饰�ʣ���������
           03  LNK-SC91-SYORIFLG       PIC 9(01).
      *    ���ť�����
           03  LNK-SC91-SRYCD          PIC X(09).
      *    ���ߣӣУ��ɣģء�1:����ʬ�λ���
           03  LNK-SC91-GMN-IDX        PIC 9(04).
      *    ���
           03  LNK-SC91-KAISU          PIC 9(03).
      *    ����
           03  LNK-SC91-SURYO          PIC 9(03).
      *    �������ο��ť�����
           03  LNK-SC91-TEISEI-AREA.
               05  LNK-SC91-TEISEI-OCC      OCCURS   400.
                   07  LNK-SC91-TEI-SRYCD   PIC X(09).
                   07  LNK-SC91-TEI-KAISU   PIC 9(03).
      *    ϫ�ҤΥ�ϥӥ�ʣ��������å����ꡢ�����������ʤ���
           03  LNK-SC91-CHKFLG         PIC 9(01).
      *    �������ֻ�����
           03  LNK-SC91-NYUCNT         PIC 9(03).
      *
      *����ޤȤỻ�������ΰ�
           03  LNK-SC91-KT01-AREA.
               05  LNK-SC91-KT01FLG         PIC X(01).
               05  LNK-SC91-KT01-OCC        OCCURS   100.
                   07  LNK-SC91-KT01-SRYCD  PIC X(09).
                   07  LNK-SC91-KT01-DAY-G.
                       09  LNK-SC91-KT01-DAY     PIC 9(03)
                                                  OCCURS   31.
      *
      *����
      *    ������
           03  LNK-SC91-MAXCNT         PIC 9(04).
      *    ���߷��
           03  LNK-SC91-CNT            PIC 9(04).
      *
      *    ������(����
           03  LNK-SC91-DAYMAXCNT      PIC 9(04).
      *    ���߷��������
           03  LNK-SC91-DAYCNT         PIC 9(04).
      *    ��������
           03  LNK-SC91-ATO-CNT        PIC 9(04).
      *
      *    ������(����
           03  LNK-SC91-WEEKMAXCNT     PIC 9(04).
      *    ���߷���ʽ���
           03  LNK-SC91-WEEKCNT        PIC 9(04).
      *
      *    �����������
           03  LNK-SC91-NYUZCNT        PIC 9(03).
      *
      *    ���顼������
           03  LNK-SC91-ERRCD          PIC X(04).
      *    ���顼���ť�å�����
           03  LNK-SC91-ERRMSG         PIC X(200).
      *    ���顼�����ɣ�
           03  LNK-SC91-ERRCD2         PIC X(04).
      *