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
      *    �ե����������ž��󥷥��ƥ���  (CPFILE-MIS.INC)      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  FILE-MIS-REC.
      *
           03  FILE-MIS-KEY.
      *
               05  FILE-MIS-HOSPNUM    PIC 9(02).
      *�ˣţ�
               05  FILE-MIS-TBL-KEY    PIC X(08).
      *Ϣ��
               05  FILE-MIS-RENNUM     PIC 9(04).
      *���롼��
               05  FILE-MIS-TBL-GROUP  PIC X(14).
      *������Ϣ��
               05  FILE-MIS-SHORI-RENNUM
                                       PIC 9(04).
      *������ɣ�
           03  FILE-MIS-SHELLID        PIC X(20).
      *����ǯ��
           03  FILE-MIS-SRYYM          PIC X(06).
      *����ǯ����
           03  FILE-MIS-SRYYMD         PIC X(08).
      *���
           03  FILE-MIS-CNT            PIC 9(07).
      *������ʬ
           03  FILE-MIS-NYUGAIKBN      PIC X(01).
      *���Ǽ���
           03  FILE-MIS-TO-FOLDER      PIC X(200).
      *��ե�����̾
           03  FILE-MIS-TO-DATA        PIC X(200).
      *�����ɶ�ʬ�ʶ��򡢣����ţգ� �����ӡݣ����
           03  FILE-MIS-CODE-TYPE      PIC X(01).
      *���ϥե饰
           03  FILE-MIS-PUTFLG         PIC X(1).
      *�����ȥ�
           03  FILE-MIS-TITLE          PIC X(100).
      *UUID
           03  FILE-MIS-TBL-UUID       PIC X(36).
      *����ǯ����
           03  FILE-MIS-CREYMD         PIC X(08).
      *����ǯ����
           03  FILE-MIS-UPYMD          PIC  X(08).
      *��������
           03  FILE-MIS-UPHMS          PIC  X(06).