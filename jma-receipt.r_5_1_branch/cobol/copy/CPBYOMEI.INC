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
      *  ��̾�ޥ�����       (CPBYOMEI.INC)  
      *****************************************************************
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01/00/01    MCC-ƣ��     01.11.09  �������쥤�����Ȥ��ѹ�     
      *  02/00/01    NACL-��ƣ    02.07.29  �������쥤�����Ȥ��ѹ�     
      *  04/06/01    NACL-��ƣ    10.10.19  �������쥤�����Ȥ��ѹ�     
      *  05/00/00    NACL-��ƣ    17.01.06  ICD10(2013)�ɲ�     
      *****************************************************************
      *01  BYOMEI-REC.
      *
           03  BYO-KEY.
      *��̾������
               05  BYO-BYOMEICD        PIC X(07).
      *��̾
           03  BYO-BYOMEI              PIC X(200).
      *��̾ʸ����
           03  BYO-BYOMEIMOJI          PIC 9(03).
      *��ά��̾
           03  BYO-TANBYOMEI           PIC X(200).
      *��ά��̾ʸ����
           03  BYO-TANBYOMEIMOJI       PIC 9(03).      
      *��̾����
           03  BYO-BYOMEIKANA          PIC X(200).
      *�ܹ��襳����
           03  BYO-IKOSAKICD           PIC X(07).
      *�������������
           03  BYO-TOKSKNCD            PIC 9(02).
      *���³��襳����
           03  BYO-NANBYOCD            PIC 9(02).
      *�����ե饰
           03  BYO-UTAGAIFLG           PIC X(01).
      *ñ�Ȼ��Ѷػ߶�ʬ
           03  BYO-TANDOKUKBN          PIC 9(02).
      *�ݸ����ᳰ��ʬ
           03  BYO-HKNSKYKBN           PIC 9(01).
      *��̾������ʬ
           03  BYO-HYOJUNCD            PIC X(08).
      *�����ʬ
           03  BYO-SAITAKUKBN          PIC X(01).
      *��̾���ѥ�����
           03  BYO-BYOMEICHGCD         PIC X(04).
      *�ɣãģ�������
           03  BYO-ICD10-1             PIC X(05).
      *�ɣãģ�������
           03  BYO-ICD10-2             PIC X(05).
      *�ɣãģ��������ʣ���������
           03  BYO-ICD10-1-2           PIC X(05).
      *�ɣãģ��������ʣ���������
           03  BYO-ICD10-2-2           PIC X(05).
      *����ǯ����
           03  BYO-SYUSAIYMD           PIC X(08).
      *�ѹ�ǯ����
           03  BYO-CHGYMD              PIC X(08).
      *�ѻ�ǯ����
           03  BYO-HAISIYMD            PIC X(08).
      *ü���ɣ�
           03  BYO-TERMID              PIC X(32).
      *���ڡ��졼���ɣ�
           03  BYO-OPID                PIC X(16).
      *����ǯ����
           03  BYO-CREYMD              PIC X(08).
      *����ǯ����
           03  BYO-UPYMD               PIC X(08).
      *��������
           03  BYO-UPHMS               PIC X(06).
