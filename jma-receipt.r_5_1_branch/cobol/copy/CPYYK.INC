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
      *****************************************************************
      *  ͽ��ޥ�����       (CPYYK.INC) 
      *****************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  YYK-REC.
           03  YYK-KEY.
      *���ŵ��أɣ�
               05  YYK-HOSPNUM          PIC 9(2).
      *��������
               05  YYK-SRYNAIYO        PIC X(02).
      *�ɥ�����������
               05  YYK-DRCD            PIC X(05).
      *ͽ��ǯ����
               05  YYK-YYKYMD          PIC X(08).
      *����ͽ�����
               05  YYK-KEYYYKTIME      PIC 9(04).
      *ͽ��ɣ�
               05  YYK-YYKID           PIC 9(02).
      *ͽ�����ʣɣġᣰ����
           03  YYK-NAIYOU1.
      *ͽ�������
               05  YYK-YYKMAXCNT       PIC 9(03).
      *ͽ����
               05  YYK-YYKCNT          PIC 9(03).
      *ͽ�����
           03  YYK-YYKTIME             PIC 9(04).
      *���ԣɣ�
           03  YYK-PTID                PIC 9(10).
      *���Ի�̾
           03  YYK-NAME                PIC X(100).
      *���Ų�
           03  YYK-SRYKA               PIC X(02).
      *ͽ���ʬ
           03  YYK-YYKKBN              PIC X(02).
      *��ǧϢ��
           03  YYK-KAKREN              PIC X(02).
      *�象��
           03  YYK-RAIINZUMI           PIC X(01).
      *ü���ɣ�
           03  YYK-TERMID              PIC  X(16).
      *���ڡ��졼���ɣ�
           03  YYK-OPID                PIC  X(16).
      *����ǯ����
           03  YYK-CREYMD              PIC  X(08).
      *����ǯ����
           03  YYK-UPYMD               PIC  X(08).
      *��������
           03  YYK-UPHMS               PIC  X(06).
