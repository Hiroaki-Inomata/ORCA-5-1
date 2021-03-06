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
      *    �ݸ�����ô��������ۼ����ߥ��֥ѥ�᥿
      *    COPY  (CPORCSSKYGET.INC)
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.07.01    NACL-����    13/10/15  �����ա��軰�Թ԰١��б�
      *  04.08.01    NACL-����    16/09/28  �����ա��軰�Թ԰١�
      *                                     �ݸ�����������å�ɽ�ε�ǽ�ɲ�
      ************************************************************
       01  ORCSSKYGETAREA.
      *
      *������
      *  ��ʬ
           03  LNK-SKYGET-KBN                           PIC X(01).
      *  ����ǯ��
           03  LNK-SKYGET-SRYYM                         PIC 9(06).
      *  �ѥ�᥿�����軰�Թ԰١˰����������ô�ۡ�����ɸ����ô�ۤν�����
      *              ���������ա��軰�Թ԰١ˡ���������
           03  LNK-SKYGET-PARA                          PIC X(01).
      *
      *�ֵ���
           03  LNK-SKYGET-OUT-AREA.
      *
      *  ����ۡʰ�����˾���
               05  LNK-SKYGET-IRYO-INF.
      *    ������
                   07  LNK-SKYGET-IRYO-NISSU            PIC 9(02).
      *    ������
                   07  LNK-SKYGET-IRYO-TOTALTEN         PIC 9(08).
      *    ������ô
                   07  LNK-SKYGET-IRYO-ICHIBUFTN        PIC 9(09).
      *    �ݸ�����ô
                   07  LNK-SKYGET-IRYO-HOKENJA          PIC 9(09).
      *    ������ô
                   07  LNK-SKYGET-IRYO-KOHFTN           PIC 9(09).
      *
      *  ����ۡʿ����˾���
               05  LNK-SKYGET-SKJ-INF.
      *    ���
                   07  LNK-SKYGET-SKJ-KAISU             PIC 9(02).
      *    �����
                   07  LNK-SKYGET-SKJ-RYOYOHI           PIC 9(09).
      *    ������ô
                   07  LNK-SKYGET-SKJ-ICHIBUFTN         PIC 9(09).
      *    �ݸ�����ô
                   07  LNK-SKYGET-SKJ-HOKENJA           PIC 9(09).
      *    ������ô
                   07  LNK-SKYGET-SKJ-KOHFTN            PIC 9(09).
      *
      *  �������ʥ쥻�ץ�����ʬ��
               05  LNK-SKYGET-KOH-INF.
                 06  LNK-SKYGET-KOH-OCC                 OCCURS 4.
                   07  LNK-SKYGET-KOH-HKNNUM            PIC X(03).
                   07  LNK-SKYGET-KOH-NISSU             PIC 9(02).
                   07  LNK-SKYGET-KOH-TOTALTEN          PIC 9(08).
                   07  LNK-SKYGET-KOH-FTNMONEY          PIC 9(09).
                   07  LNK-SKYGET-KOH-KAISU             PIC 9(02).
                   07  LNK-SKYGET-KOH-RYOYOHI           PIC 9(09).
                   07  LNK-SKYGET-KOH-SHOKUJIFTN        PIC 9(09).
      *
      *  �������ʸ������������ʬ��
               05  LNK-SKYGET-KOHETC-INF.
                 06  LNK-SKYGET-KOHETC-OCC              OCCURS 4.
                   07  LNK-SKYGET-KOHETC-HKNNUM         PIC X(03).
                   07  LNK-SKYGET-KOHETC-NISSU          PIC 9(02).
                   07  LNK-SKYGET-KOHETC-TOTALTEN       PIC 9(08).
                   07  LNK-SKYGET-KOHETC-FTNMONEY       PIC 9(09).
                   07  LNK-SKYGET-KOHETC-KAISU          PIC 9(02).
                   07  LNK-SKYGET-KOHETC-RYOYOHI        PIC 9(09).
                   07  LNK-SKYGET-KOHETC-SHOKUJIFTN     PIC 9(09).
      *
      *  ����ե饰�ʥ쥻�ץȤȸ�������������ǯ��ۤʤ����
      *  �ʣ���Ʊ�����ϸ��������ʤ��������쥻�ץȤΤߡ��������������Τߡ�
               05  LNK-SKYGET-SEIKYUFLG                 PIC 9(01).
      *
      *�ֵ��ͣ�
           03  LNK-SKYGET-OUT2-AREA.
      *      (1)���ݤμ���ʬ�ʲ�
      *      (2)���ݤμ���ʬ�ʲ�
      *      (3)�������μ���ʬ�ʲ�
      *      (4)����������
      *      (5)�����ݸ�
      *      (6)����
      *      (7)ϫ��
      *      (8)������
      *      (9)����
      *      (10)�軰�Թ԰٤μ���ʬ�ʲ�
             04  LNK-SKYGET-OUT2-OCC                    OCCURS 10.
      *        (1)�����β���Ǽ(2)����(3)�������ܼ�Ǽ
               05  LNK-SKYGET-OUT2-OCC2                 OCCURS 3.
      *
      *  ���
                 06  LNK-SKYGET-KENSU                   PIC 9(06).
      *
      *  ����ۡʰ�����˾���
                 06  LNK-SKYGET-IRYO2-INF.
      *    ������
                   07  LNK-SKYGET-IRYO2-NISSU           PIC 9(06).
      *    ������
                   07  LNK-SKYGET-IRYO2-TOTALTEN        PIC 9(08).
      *    ������ô
                   07  LNK-SKYGET-IRYO2-ICHIBUFTN       PIC 9(09).
      *    �ݸ�����ô
                   07  LNK-SKYGET-IRYO2-HOKENJA         PIC 9(09).
      *    ������ô
                   07  LNK-SKYGET-IRYO2-KOHFTN          PIC 9(09).
      *
      *  ����ۡʿ����˾���
                 06  LNK-SKYGET-SKJ2-INF.
      *    ���
                   07  LNK-SKYGET-SKJ2-KAISU            PIC 9(06).
      *    �����
                   07  LNK-SKYGET-SKJ2-RYOYOHI          PIC 9(09).
      *    ������ô
                   07  LNK-SKYGET-SKJ2-ICHIBUFTN        PIC 9(09).
      *    �ݸ�����ô
                   07  LNK-SKYGET-SKJ2-HOKENJA          PIC 9(09).
      *    ������ô
                   07  LNK-SKYGET-SKJ2-KOHFTN           PIC 9(09).
      *
      *  ����ʬ
                 06  LNK-SKYGET-JIHI                    PIC 9(09).
      *  ���ȶ��
                 06  LNK-SKYGET-DISCOUNT-MONEY          PIC S9(09).
      *  Ĵ�����
                 06  LNK-SKYGET-CHOSEI                  PIC S9(09).
      *  Ĵ���⣱
                 06  LNK-SKYGET-CHOSEI1                 PIC S9(09).
      *  Ĵ���⣲
                 06  LNK-SKYGET-CHOSEI2                 PIC S9(09).
      *  ü��Ĵ�����
                 06  LNK-SKYGET-HASU                    PIC S9(09).
      *
      *����������
           03  LNK-SKYGET-RC                            PIC 9(02).
