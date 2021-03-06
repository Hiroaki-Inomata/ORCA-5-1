      ******************************************************************
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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ******************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �Ģɼ
      *  ����ݡ��ͥ��̾  : ���Ź԰��̽���ɽ
      *  ������            : 
      *  Maj/Min/Rev  ������  ����        ����
      *   01.00.00    ����    2011/02/14  ��������
      *   01.00.01    ���    2014/05/22  ����������������������̤򾮿�����2�̤ޤ��б�
      *   01.00.02    ���    2017/06/30  �����ۡ����������η�����ѹ�
      *******************************************************************
       01  TA02TMP-REC.
      *    KEY����
           03  TA02TMP-KEY.
      *    ������ʬ
             05  TA02TMP-PRTKBN           PIC 9(05).
      *    �������
             05  TA02TMP-SORTKBN          PIC 9(02).
      *    ���Ŷ�ʬ
             05  TA02TMP-SRYKBN           PIC X(02).
      *    ����̾
             05  TA02TMP-SRYNAME          PIC X(40).
      *    ���ť�����
             05  TA02TMP-SRYCD            PIC X(09).
      *    ����
             05  TA02TMP-YAKKA            PIC S9(07)V9(02).
      *    ñ��
             05  TA02TMP-TANKA            PIC S9(07)V9(02).
      *    ���٥ơ��֥�
           03  TA02TMP-SRY-TBL.
      *    ������
             05  TA02TMP-GAIRAI-KEN       PIC 9(07)V9(02).
      *    �������
             05  TA02TMP-NYUIN-KEN        PIC 9(07)V9(02).
      *    ��׷��
             05  TA02TMP-TOTAL-KEN        PIC 9(08)V9(02).
      *    ���������
             05  TA02TMP-GAIRAI-SIYO      PIC 9(07)V9(02).
      *    ����������
             05  TA02TMP-NYUIN-SIYO       PIC 9(07)V9(02).
      *    ��׻�����
             05  TA02TMP-TOTAL-SIYO       PIC 9(08)V9(02).
      *    ������
             05  TA02TMP-GAIRAI-KIN       PIC 9(09).
      *    �������
             05  TA02TMP-NYUIN-KIN        PIC 9(09).
      *    ��׶��
             05  TA02TMP-TOTAL-KIN        PIC 9(09).
