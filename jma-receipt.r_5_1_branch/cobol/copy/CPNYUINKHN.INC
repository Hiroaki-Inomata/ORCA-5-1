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
      *    ����������ޥ���         COPY (CPNYUINKHN.INC)        *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  NYUINKHN-REC.
           03  NYUKHN-KEY.
      *�����������ť�����
               05  NYUKHN-KHNSRYCD     PIC  X(09).
      *ͭ������ǯ����
               05  NYUKHN-YUKOSTYMD    PIC  X(08).
      *ͭ����λǯ����
               05  NYUKHN-YUKOEDYMD    PIC  X(08).
      *������ť�����
           03  NYUKHN-INF.
      *����Ϸ�Ͷ�ʬ
      *�ʣ������̡�����Ϸ�͡�
               05  NYUKHN-IPROUKBN     PIC  X(01).
      *���ߴ���ʬ
      *���򡧻��ߴ����θ���ʤ�
      *  �������굡ǽ�±�
      *  ����ͭ�����Ž�����̴Ǹ���ϽФ���
      *  ����ͭ�����Ž�ǴǸ�գ��Ͱʾ�
      *  ����ͭ�����Ž��ʣ����������Ŭ��
      *  ����ͭ�����Ž��ʣ����������Ŭ�硦�Ǹ�գ��Ͱʾ�
               05  NYUKHN-SSTKIJUNKBN
                                       PIC  X(01).
      *������̶�ʬ
               05  NYUKHN-BTUSBTKBN    PIC  X(02).
      *ʿ�Ѻ߱�������ʬ
               05  NYUKHN-HEIKINZAIKBN PIC  X(02).
      *�Ǹ����ֶ�ʬ
               05  NYUKHN-KANGOHAICHIKBN
                                       PIC  X(02).
      *�Ǹ����Ψ��ʬ
               05  NYUKHN-KANGOHIRITUKBN
                                       PIC  X(02).
      *�Ǹ�������ֶ�ʬ
               05  NYUKHN-KANGOHOJOHAICHIKBN
                                       PIC  X(02).
      *��������
               05  NYUKHN-KHNTEN       PIC  9(06).
      *�ø������ť����ɾ���
           03  NYUKHN-KAGENCDINF.
      *�ø������ť����ɣ��ʣ����������
               05  NYUKHN-KAGENSRYCD1  PIC  X(09).
      *�ø������ť����ɣ��ʣ����������
               05  NYUKHN-KAGENSRYCD2  PIC  X(09).
      *�ø������ť����ɣ��ʣ������ʾ壹���������
               05  NYUKHN-KAGENSRYCD3  PIC  X(09).
      *�ø������ť����ɣ��ʣ������ʾ��
               05  NYUKHN-KAGENSRYCD4  PIC  X(09).
      *�ø������ť����ɣ��ʣ������������
               05  NYUKHN-KAGENSRYCD5  PIC  X(09).
      *�ø������ť����ɣ��ʣ��������ʾ壱ǯ�����
               05  NYUKHN-KAGENSRYCD6  PIC  X(09).
      *�ø������ť����ɣ��ʣ��������ʾ��
               05  NYUKHN-KAGENSRYCD7  PIC  X(09).
      *�ø������ť����ɾ�������
           03  NYUKHN-KAGENCD-R     REDEFINES  NYUKHN-KAGENCDINF.
               05  NYUKHN-KAGENCD-T         OCCURS   7.
                 07  NYUKHN-KAGENCD         PIC  X(09).
      *ü���ɣ�
           03  NYUKHN-TERMID           PIC  X(16).
      *���ڡ��졼���ɣ�
           03  NYUKHN-OPID             PIC  X(16).
      *����ǯ����
           03  NYUKHN-CREYMD           PIC  X(08).
      *����ǯ����
           03  NYUKHN-UPYMD            PIC  X(08).
      *��������
           03  NYUKHN-UPHMS            PIC  X(06).
      *������
           03  NYUKHN-UP-KEY.
               05  NYUKHN-UPKHNSRYCD   PIC  X(09).
               05  NYUKHN-UPYUKOSTYMD  PIC  X(08).
               05  NYUKHN-UPYUKOEDYMD  PIC  X(08).
