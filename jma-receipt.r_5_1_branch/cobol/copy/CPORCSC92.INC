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
      *    ���ť����ɥ����å���   COPY (CPORCSC92.INC)
      *                              (ORCSC92 �ǻ��ѡ�
      *
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCSC92AREA.
      *
      *    1:�ɲ�ʬ���ɲãϣ˻���'��'���ѹ��ˣ��������å��Τ�
           03  LNK-SC92-KBN            PIC X(01).
      *    ���������ͤ���
           03  LNK-SC92-INFLG          PIC X(01).
      *    �����ե饰�ʣ���������
           03  LNK-SC92-SYORIFLG       PIC 9(01).
      *    1:���û������å��ʤ��ʲ������Ǥ����Ļ��û�����
           03  LNK-SC92-CHUKSN         PIC X(01).
      *    ����:�ٹ𥨥顼�ʤ��ʼ�ưȯ�������оݤȤ����
      *    1:�ٹ𥨥顼��������ϻ���
           03  LNK-SC92-KEIFLG         PIC X(01).
      *
      *    ���ť�����
           03  LNK-SC92-SRYCD          PIC X(09).
      *    �оݡ��ɲãɣģ�
           03  LNK-SC92-GMN-IDX        PIC 9(04).
      *    �����У�̾
           03  LNK-SC92-PG             PIC X(04).
      *
           03  LNK-SC92-AREA.
      *        ǯ�� 
               05  LNK-SC92-NENREI.
                   07  LNK-SC92-NENREI-YY     PIC 9(03).
                   07  LNK-SC92-NENREI-MM     PIC 9(02).
                   07  LNK-SC92-NENREI-DD     PIC 9(02).
      *        ������̤���ƿǲʻ���ͭ��ʽ�ǡ��ƿǤΤ߻��ѡ�
               05  LNK-SC92-15MIMAN-ARI       PIC  9(01).
      *        Ϸ�Ͷ�ʬ
               05  LNK-SC92-ROUJIN            PIC 9(01).
      *        �ǽ����ǲ�
               05  LNK-SC92-LSTSRYKA          PIC  X(02).
      *        �ǽ����ǲ�
               05  LNK-SC92-LSTSRYKAMEI       PIC  X(20).
      *        ��������̵ͭ
               05  LNK-SC92-NYUINFLG          PIC  9(01).
      *    �������ο��ť�����
           03  LNK-SC92-TEISEI-AREA.
               05  LNK-SC92-TEISEI-OCC      OCCURS   400.
                   07  LNK-SC92-TEI-SRYCD   PIC X(09).
                   07  LNK-SC92-TEI-KAISU   PIC 9(03).
      *
           03  LNK-SC92-ERRAREA.
      *        ���顼�����ɡ�����Ŭ�Ѷ�ʬ��
               05  LNK-SC92-ERRCD1         PIC X(04).
      *        ���顼�����ɡ�Ϸ��Ŭ�Ѷ�ʬ��
               05  LNK-SC92-ERRCD2         PIC X(04).
      *        ���顼�����ɡʻ�������
               05  LNK-SC92-ERRCD3         PIC X(04).
      *        ���顼�����ɡʻ��ߴ��Ŭ��ʬ��
               05  LNK-SC92-ERRCD4         PIC X(04).
      *        ���顼�����ɡ��о�ǯ���
               05  LNK-SC92-ERRCD5         PIC X(04).
      *        ���顼�����ɡ��¾�����
               05  LNK-SC92-ERRCD6         PIC X(04).
      *        ���顼�����ɡ�ʻ�ѻ����
               05  LNK-SC92-ERRCD7         PIC X(04).
      *        ���顼������
               05  LNK-SC92-ERRCD8         PIC X(04).
      *        ���顼������
               05  LNK-SC92-ERRCD9         PIC X(04).
      *        ���顼������
               05  LNK-SC92-ERRCD10        PIC X(04).
           03  LNK-SC92-ERRAREA-R      REDEFINES   LNK-SC92-ERRAREA.
               05  LNK-SC92-ERRCD          PIC X(04)
                                           OCCURS  10.
      *    ���顼��å�������ʻ�ѻ����
           03  LNK-SC92-ERRMSG         PIC X(100).
      *    ʻ�ѻ���ٹ�
           03  LNK-SC92-KEIARI         PIC X(01).
      *    �����ʻ�ѻ��ꥨ�顼
           03  LNK-SC92-SYOKBN         PIC X(01).
      *
      *    �����͡�ORCSC97�ˡʿ��Ź԰ٲ������ϻ��Τߡ�
           03  LNK-SC92-INPUT-G        PIC X(100).
           03  LNK-SC92-TIMEFLG        PIC X(01).
      *
      *����ޤȤỻ�������ΰ�
           03  LNK-SC92-KT01-AREA.
               05  LNK-SC92-KT01FLG         PIC X(01).
               05  LNK-SC92-KT01-OCC        OCCURS   100.
                   07  LNK-SC92-KT01-SRYCD  PIC X(09).
                   07  LNK-SC92-KT01-DAY-G.
                       09  LNK-SC92-KT01-DAY     PIC 9(03)
                                                  OCCURS   31.
      *