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
      *  ������̾�ޥ�����       (CPUSERBYOMEI.INC)
      *****************************************************************
      *  ��������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    03/10/28  ��̾�����ɤ򣲣����ѹ�
      *                                     ��̾�����ɿ��򣹡ʣ��ˤ��ѹ�
      *                                     ��̾���̡�������̾�����ɡ�
      *                                     �������̥����ɤ��ɲ�
      *****************************************************************
      *
      *01  USERBYOMEI-REC.
           03  USBYO-KEY.
      *���ŵ��أɣ�
               05  USBYO-HOSPNUM        PIC 9(2).
      *�����ɼ���
               05  USBYO-CDSYU         PIC X(01).
      *��̾���ϥ�����
               05  USBYO-BYOMEIINPUTCD PIC X(20).
      *ʬ�ॳ����
           03  USBYO-BUNRUICD          PIC X(03).
      *ɽ��Ϣ��
           03  USBYO-DSPSEQ            PIC 9(02).
      *��̾�����ɿ�
           03  USBYO-BYOMEICDSU        PIC 9(02).
      *��̾������
           03  USBYO-BYOMEICD-G.
               05  USBYO-BYOMEICD-1    PIC X(07).
               05  USBYO-BYOMEICD-2    PIC X(07).
               05  USBYO-BYOMEICD-3    PIC X(07).
               05  USBYO-BYOMEICD-4    PIC X(07).
               05  USBYO-BYOMEICD-5    PIC X(07).
               05  USBYO-BYOMEICD-6    PIC X(07).
               05  USBYO-BYOMEICD-7    PIC X(07).
               05  USBYO-BYOMEICD-8    PIC X(07).
               05  USBYO-BYOMEICD-9    PIC X(07).
               05  USBYO-BYOMEICD-10   PIC X(07).
               05  USBYO-BYOMEICD-11   PIC X(07).
               05  USBYO-BYOMEICD-12   PIC X(07).
               05  USBYO-BYOMEICD-13   PIC X(07).
               05  USBYO-BYOMEICD-14   PIC X(07).
               05  USBYO-BYOMEICD-15   PIC X(07).
               05  USBYO-BYOMEICD-16   PIC X(07).
               05  USBYO-BYOMEICD-17   PIC X(07).
               05  USBYO-BYOMEICD-18   PIC X(07).
               05  USBYO-BYOMEICD-19   PIC X(07).
               05  USBYO-BYOMEICD-20   PIC X(07).
               05  USBYO-BYOMEICD-21   PIC X(07).
      *
           03  USBYO-BYOMEICD-R        REDEFINES   USBYO-BYOMEICD-G.
               05  USBYO-BYOMEICD      PIC X(07)
                                       OCCURS   21.
      *�����ե饰
           03  USBYO-UTAGAIFLG         PIC X(01).
      *������ʬ
           03  USBYO-MANSEIKBN         PIC 9(02).
      *��̾
           03  USBYO-BYOMEI            PIC X(160).
      *��̾ʸ����
           03  USBYO-BYOMEIMOJI        PIC 9(02).
      *�������̾
           03  USBYO-CHARTBYOMEI       PIC X(160).
      *�������̾ʸ����
           03  USBYO-CHARTBYOMEIMOJI   PIC 9(02).
      *��­������
           03  USBYO-HOSOKU-COMMENT    PIC X(80).
      *��̾�Խ��ե饰
           03  USBYO-BYOMEIHENFLG      PIC X(01).
      *�ݸ���̾�ե饰
           03  USBYO-HKNBYOMEIFLG      PIC X(01).
      *��̾����
           03  USBYO-BYOMEISBT         PIC X(01).
      *������̾������
           03  USBYO-KHNBYOMEICD       PIC X(07).
      *�������̥�����
           03  USBYO-KHNBUICD-G.
               05  USBYO-KHNBUICD      PIC X(07)   OCCURS  3.
      *ü���ɣ�
           03  USBYO-TERMID            PIC X(16).
      *���ڡ��졼���ɣ�
           03  USBYO-OPID              PIC X(16).
      *����ǯ����
           03  USBYO-CREYMD            PIC X(08).
      *����ǯ����
           03  USBYO-UPYMD             PIC X(08).
      *��������
           03  USBYO-UPHMS             PIC X(06).