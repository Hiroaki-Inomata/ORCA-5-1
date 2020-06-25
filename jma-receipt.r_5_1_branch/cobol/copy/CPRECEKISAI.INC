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
      *      �쥻�ץȵ������Υޥ�����     COPY (CPRECEKISAI.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  RKISAI-REC.
           03  RKISAI-KEY.
      *        ����
               05  RKISAI-ITEMNO               PIC 9(04).
      *        ����
               05  RKISAI-EDANO                PIC 9(02).
      *        ����ǯ��
               05  RKISAI-STYM                 PIC X(06).
      *        ��λǯ��
               05  RKISAI-EDYM                 PIC X(06).
      *        ���Ź԰٥�����
               05  RKISAI-SRYCD                PIC X(09).
      *        Ϣ��
               05  RKISAI-RENNUM               PIC 9(02).
      *    �����ȥ�����
           03  RKISAI-C-CD                     PIC X(09).
      *    ���Ԥξ��֥�����
           03  RKISAI-JYOTAICD                 PIC X(03).
      *    �Ǻܰ��ֶ�ʬ
           03  RKISAI-KEISAIKBN                PIC 9(01).
      *    ��ʬ
           03  RKISAI-KUBUN                    PIC X(64).
      *    ����ʬ
           03  RKISAI-JYOKEN                   PIC 9(02).
      *    ������ͳ������
           03  RKISAI-HISANTEI                 PIC 9(01).
      *    ������ʬ
      *      0:
      *      1:���ϡ�������
      *      2:���ϡʼ�ưȯ����
      *      3:�쥻�ץȡʼ�ư���ܡ�
           03  RKISAI-SYORIKBN                 PIC 9(01).
      *    ���Ź԰�̾����
           03  RKISAI-MEISYOETC                PIC X(300).
      *    �ѹ�ǯ����
           03  RKISAI-CHGYMD                   PIC X(08).
      *    �ѻ�ǯ����
           03  RKISAI-HAISIYMD                 PIC X(08).
      *    ����ǯ����
           03  RKISAI-CREYMD                   PIC X(08).
      *    ����ǯ����
           03  RKISAI-UPYMD                    PIC X(08).
      *    ��������
           03  RKISAI-UPHMS                    PIC X(06).
      *    ������ʬ
           03  RKISAI-NYUGAIKBN                PIC X(01).
      *    ������
           03  RKISAI-KAISU                    PIC 9(03).
      *������
           03  RKISAI-UP-KEY.
               05  RKISAI-UPITEMNO             PIC 9(04).
               05  RKISAI-UPEDANO              PIC 9(02).
               05  RKISAI-UPSTYM               PIC X(06).
               05  RKISAI-UPEDYM               PIC X(06).
               05  RKISAI-UPSRYCD              PIC X(09).
               05  RKISAI-UPRENNUM             PIC 9(02).