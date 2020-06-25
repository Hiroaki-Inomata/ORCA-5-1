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
      *    ������Ģ�ǡ���Ϣ��       COPY (CPOKUSURIDATA.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  OKUSURI-DATA-REC.
      *����
           03  OKUDAT-KEY.
      *�ǡ�������
               05  OKUDAT-DATA-KEY          PIC  9(16).
      *�ң��ֹ�
               05  OKUDAT-RP                PIC  9(03).
      *�ң���Ϣ��
               05  OKUDAT-RP-SEQ            PIC  9(03).
      *�ǡ�������
           03  OKUDAT-DATA-CLASS            PIC  9(03).
      *̾��
           03  OKUDAT-NAME                  PIC  X(200).
      *����
           03  OKUDAT-SURYO                 PIC  9(5)V9(5).
      *ñ��̾
           03  OKUDAT-TANINAME              PIC  X(20).
      *�����ɶ�ʬ
           03  OKUDAT-CODE-CLASS            PIC  9(02).
      *������
           03  OKUDAT-CODE                  PIC  X(20).
      *ü���ɣ�
           03  OKUDAT-TERMID                PIC  X(16).
      *���ڡ��졼���ɣ�
           03  OKUDAT-OPID                  PIC  X(16).
      *����ǯ����
           03  OKUDAT-CREYMD                PIC  X(08).
      *����ǯ����
           03  OKUDAT-UPYMD                 PIC  X(08).
      *��������
           03  OKUDAT-UPHMS                 PIC  X(06).