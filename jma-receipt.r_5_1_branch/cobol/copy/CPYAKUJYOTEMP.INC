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
      *  ���޾���       (CPYAKUJYOTEMP.INC) 
      *****************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  TEMP-REC.
           03  TEMP-KEY.
               05  TEMP-HOSPNUM         PIC 9(2).
      *ü���ɣ�
               05  TEMP-TERMID          PIC X(64).
      *�����ʥ�����
               05  TEMP-SRYCD           PIC X(09).
      *�٣ʥ�����
               05  TEMP-YJCD            PIC X(12).
      *Ϣ��
               05  TEMP-RENNUM          PIC 9(2).
      *��
           03  TEMP-SHAPE               PIC X(360).
      *��
           03  TEMP-COLOR               PIC X(360).
      *����
           03  TEMP-MARK                PIC X(360).
      *��ǽ����
           03  TEMP-EFFECT              PIC X(2000).
      *���ջ���
           03  TEMP-CAUTION             PIC X(4000).
      *��¤���̾
           03  TEMP-MAKE-COMPANY        PIC X(400).
      *������̾
           03  TEMP-SALE-COMPANY        PIC X(400).
      *�����ե�����̾��
           03  TEMP-PHOTO1-FILENAME     PIC X(256).
      *�����ե�����̾��
           03  TEMP-PHOTO2-FILENAME     PIC X(256).
      *�����ե�����̾��
           03  TEMP-PHOTO3-FILENAME     PIC X(256).
      *����ǯ����
           03  TEMP-CREYMD              PIC  X(08).
      *̾��
           03  TEMP-NAME                PIC X(200).