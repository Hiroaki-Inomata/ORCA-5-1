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
      *----------------------------------------------------------------*
      *    ���������ȥ����ɥơ��֥�
      *                            COPY (CMCOMGENGO.INC)               *
      *----------------------------------------------------------------*
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       01  TBL-GENGO-COM-AREA.
      *    �ߵް��Ŵ�������
           03  FILLER              PIC X(11)   VALUE   "4 840000020".
           03  FILLER              PIC X(11)   VALUE   "5 840000520".
      *    ��̿�ߵ�����
           03  FILLER              PIC X(11)   VALUE   "4 840000021".
           03  FILLER              PIC X(11)   VALUE   "5 840000521".
      *    ����ܿ��»���
           03  FILLER              PIC X(11)   VALUE   "4 840000034".
           03  FILLER              PIC X(11)   VALUE   "5 840000534".
      *    ������
           03  FILLER              PIC X(11)   VALUE   "4 840000014".
           03  FILLER              PIC X(11)   VALUE   "5 840000514".
      *    ���곫��
           03  FILLER              PIC X(11)   VALUE   "4 840000056".
           03  FILLER              PIC X(11)   VALUE   "5 840000556".
      *    ���������裱���ܥ����󥻥��
           03  FILLER              PIC X(11)   VALUE   "4 840000104".
           03  FILLER              PIC X(11)   VALUE   "5 840000604".
      *    ���¼�����ո������
           03  FILLER              PIC X(11)   VALUE   "4 840000011".
           03  FILLER              PIC X(11)   VALUE   "5 840000511".
      *    ����³�˽��¼�����ո������
           03  FILLER              PIC X(11)   VALUE   "4 840000012".
           03  FILLER              PIC X(11)   VALUE   "5 840000512".
      *    ���ʶ�������������
           03  FILLER              PIC X(11)   VALUE   "4 840000023".
           03  FILLER              PIC X(11)   VALUE   "5 840000523".
      *    �������ݣ����ܰʹ�
           03  FILLER              PIC X(11)   VALUE   "4 840000120".
           03  FILLER              PIC X(11)   VALUE   "5 840000620".
      *    �͹���¡Ƴ����
           03  FILLER              PIC X(11)   VALUE   "4 840000007".
           03  FILLER              PIC X(11)   VALUE   "5 840000507".
      *    �����ʵ����������������������곫��
           03  FILLER              PIC X(11)   VALUE   "4 840000112".
           03  FILLER              PIC X(11)   VALUE   "5 840000612".
      *    ������ʶ�������������
           03  FILLER              PIC X(11)   VALUE   "4 840000022".
           03  FILLER              PIC X(11)   VALUE   "5 840000522".
      *    ¤�촴��˦�ܿ��»���
           03  FILLER              PIC X(11)   VALUE   "4 840000133".
           03  FILLER              PIC X(11)   VALUE   "5 840000633".
      *    �ࡡ��
           03  FILLER              PIC X(11)   VALUE   "4 840000013".
           03  FILLER              PIC X(11)   VALUE   "5 840000513".
      *    �о�Ĵ��
           03  FILLER              PIC X(11)   VALUE   "4 840000093".
           03  FILLER              PIC X(11)   VALUE   "5 840000593".
      *    �������޼��Ŵ����������
           03  FILLER              PIC X(11)   VALUE   "4 840000038".
           03  FILLER              PIC X(11)   VALUE   "5 840000538".
      *    ưŪ���ֳ���
           03  FILLER              PIC X(11)   VALUE   "4 840000025".
           03  FILLER              PIC X(11)   VALUE   "5 840000525".
      *    ưŪ���ֻ���
           03  FILLER              PIC X(11)   VALUE   "4 840000024".
           03  FILLER              PIC X(11)   VALUE   "5 840000524".
      *    ������
           03  FILLER              PIC X(11)   VALUE   "4 840000088".
           03  FILLER              PIC X(11)   VALUE   "5 840000588".
      *    ������������ô�⸺��ǧ���
           03  FILLER              PIC X(11)   VALUE   "4 840000033".
           03  FILLER              PIC X(11)   VALUE   "5 840000533".
      *    ʸ������
           03  FILLER              PIC X(11)   VALUE   "4 840000111".
           03  FILLER              PIC X(11)   VALUE   "5 840000611".
      *    ǯ����
           03  FILLER              PIC X(11)   VALUE   "4 840000061".
           03  FILLER              PIC X(11)   VALUE   "5 840000561".
      *    ǯ��
           03  FILLER              PIC X(11)   VALUE   "4 840000060".
           03  FILLER              PIC X(11)   VALUE   "5 840000560".
      *    ��ɻ���
           03  FILLER              PIC X(11)   VALUE   "4 840000117".
           03  FILLER              PIC X(11)   VALUE   "5 840000617".
      *    �ޥ���֥饱�å�ˡ����
           03  FILLER              PIC X(11)   VALUE   "4 840000026".
           03  FILLER              PIC X(11)   VALUE   "5 840000526".
      *    ������Ʊ�ս����
           03  FILLER              PIC X(11)   VALUE   "4 840000078".
           03  FILLER              PIC X(11)   VALUE   "5 840000578".
      *    Ϣ³�ȹԼ�ʢ����ήƳ����
           03  FILLER              PIC X(11)   VALUE   "4 840000008".
           03  FILLER              PIC X(11)   VALUE   "5 840000508".
      *R01.5.7
      *    �顡��
           03  FILLER              PIC X(11)   VALUE   "4 840800003".
           03  FILLER              PIC X(11)   VALUE   "5 840800503".
      *
       01  TBL-GENGO-COM-AREA-R    REDEFINES   TBL-GENGO-COM-AREA.
           03  TBL-GENGO-COM-OCC   OCCURS   58
                                   INDEXED  IDX-GENGO.
      *        4:ʿ����5������
               05  TBL-GENGO-KBN       PIC X(01).
               05  TBL-YOBI1           PIC X(01).
               05  TBL-GENGO-SRYCD     PIC X(09).
      *
      *
