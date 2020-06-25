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
      *-------------------------------------------------*
      *    ������Ź԰٥ޥ���    COPY (CPWKSRYACT.INC)
      *
      *-------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.05.00    NACL-¿��Ǽ  09/10/02  ���϶�ʬ���������Ͽ����ɲ�
      *  04.05.00    NACL-¿��Ǽ  10/01/07  �޼��̶�ʬ�ɲ�
      *  04.07.00    NACL-¿��Ǽ  11/10/20  Ʊ����������ʬ�ɲ�
      *  05.00.00    NACL-¿��Ǽ  20/02/XX  �����ȡ������ͷ���ѹ�
      ************************************************************
      *01  WKSRYACT-REC.
           03  WKSRY-KEY.
      *���ŵ��أɣ�
               05  WKSRY-HOSPNUM         PIC 9(2).
      *������ʬ
               05  WKSRY-NYUGAIKBN      PIC  X(01).
      *���ԣɣ�
               05  WKSRY-PTID           PIC  9(10).
      *���Ų�
               05  WKSRY-SRYKA          PIC  X(02).
      *������
               05  WKSRY-SRYYMD         PIC  X(08).
      *�ݸ��ȹ礻
               05  WKSRY-HKNCOMBI       PIC  9(04).
      *Ʊ��������ʬ
               05  WKSRY-DOUJITSUKBN    PIC  X(01).
      *���ֹ�
               05  WKSRY-ZAINUM         PIC  9(08).
      *Ϣ�ֹ�
               05  WKSRY-RENNUM         PIC  9(02).
      *�ɥ�����������
           03  WKSRY-DRCD               PIC  X(05).
      *���ż��̶�ʬ
           03  WKSRY-SRYSYUKBN          PIC  X(03).
      *���Ŷ�ʬ
           03  WKSRY-SRYKBN             PIC  X(02).
      *������
           03  WKSRY-JIHIMONEYTOTAL     PIC  9(07).
      *���Ź԰٥ơ��֥�
           03  WKSRY-SINRYO-TBL-G.
             04    WKSRY-SINRYO-TBL      OCCURS   5.
               05  WKSRY-SRYCD           PIC  X(09).
               05  WKSRY-SRYSURYO        PIC  9(05)V9(05).
               05  WKSRY-SRYKAISU        PIC  9(03).
               05  WKSRY-ZAIKAISU        PIC  9(08).
      **��ư�����ʬ
               05  WKSRY-AUTOKBN        PIC  X(01).
      **���ϥ������ֹ�
               05  WKSRY-INPUTNUM       PIC  9(03).
      **���ϥ�����
               05  WKSRY-INPUTCOMENT    PIC  X(140).
      **������
               05  WKSRY-INPUTCHI-G.
                   07  WKSRY-INPUTCHI   PIC  X(10)   OCCURS  5.
      *���ϥ�����
               05  WKSRY-INPUTCD        PIC  X(54).
      *������
               05  WKSRY-JIHIMONEY      PIC  9(07).
      *
      *���϶�ʬ�ʣ����ط������Ȼؼ�����������������ؼ���
               05  WKSRY-INPUTKBN       PIC  X(01).
      *�������Ͽ���
               05  WKSRY-KANSURYO       PIC  9(05)V9(05).
      *
      *��������
           03  WKSRY-ZAITENKEI          PIC  9(08).
      *�޲����
           03  WKSRY-ZAIKAIKEI          PIC  9(07).
      *�굻������
           03  WKSRY-SYUTEN1             PIC  9(07).
      *�굻������
           03  WKSRY-SYUTEN2             PIC  9(07).
      *��������
           03  WKSRY-YKZTEN              PIC  9(07).
      *�������
           03  WKSRY-KIZAITEN            PIC  9(07).
      *Ʊ��������ɼ�ֹ�
           03  WKSRY-DOUJI-DENPNUM      PIC  9(07).
      *Ʊ������Ϣ��
           03  WKSRY-DOUJI-RENNUM       PIC  9(01).
      *��³��ʬ
           03  WKSRY-CONTKBN            PIC  X(01).
      *�޼��̶�ʬ�ʣ�����ɾ����������ʬ��
           03  WKSRY-ZAIKBN             PIC  9(03).
      *������ե饰
           03  WKSRY-MOD-FLG            PIC  9(01).
      *�Żҥ���ƥե饰
           03  WKSRY-KARTE-FLG          PIC  9(01).
      *�Żҥ���ƥ���
           03  WKSRY-KARTE-KEY          PIC  X(36).
      *ü���ɣ�
           03  WKSRY-TERMID             PIC  X(16).
      *���ڥ졼���ɣ�
           03  WKSRY-OPID               PIC  X(16).
      *����ǯ����
           03  WKSRY-CREYMD             PIC  X(08).
      *����ǯ����
           03  WKSRY-UPYMD              PIC  X(08).
      *��������
           03  WKSRY-UPHMS              PIC  X(06).
      *