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
      *         �����å��ޥ�����     (CPCK1102.INC)              *
      *         �ʣ���������                                     *
      *         �ǡ��������å���ǽ����                         *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  CK4-1102-REC.
           03  CK4-1102-KEY.
               05  CK4-1102-HOSPNUM        PIC 9(2).
      *����������
               05  CK4-1102-KANRICD        PIC  X(04).
      *��ʬ������
               05  CK4-1102-KBNCD          PIC  X(08).
      *ͭ������ǯ����
               05  CK4-1102-STYUKYMD       PIC  X(08).
      *ͭ����λǯ����
               05  CK4-1102-EDYUKYMD       PIC  X(08).
      *���ԣɣ�
           03  CK4-1102-PTID               PIC  9(10).
      *�����ơ��֥�
           03  CK4-1102-TBL.
      *�ݸ����ֹ�
               05  CK4-1102-HKNJANUM       PIC  X(08).
      *�ݸ����ֹ�ʸ����Ĺ
               05  CK4-1102-HKNJANUMLEN    PIC  9(03).
      *���
               05  CK4-1102-JYKNKBN        PIC  X(01).
      *�ݸ��μ���
               05  CK4-1102-HKNNUM         PIC  X(03).
      *��������å�
               05  CK4-1102-KIGOCHK        PIC  X(01).
      *��������å��ѥ�����
               05  CK4-1102-KGCHKPTN       PIC  X(01).
      *������������å�
               05  CK4-1102-KGSUJI         PIC  X(01).
      *������������å�
               05  CK4-1102-KGKANJI        PIC  X(01).
      *����ѻ������å�
               05  CK4-1102-KGABT          PIC  X(01).
      *����ϥ��ե�Ĺ�������å�
               05  CK4-1102-KGHYPN         PIC  X(01).
      *�����̥����å�
               05  CK4-1102-KGKAKKO        PIC  X(01).
      *���椽��¾�����å�
               05  CK4-1102-KGETC          PIC  X(01).
      *�������̤���ϥ����å�
               05  CK4-1102-KGKANJINASHI   PIC  X(01).
      *�ֹ����̵�������å�
               05  CK4-1102-NMSUJINASHI    PIC  X(01).
      *�ֹ棱ʸ���ܥ����å��ѥ�����
               05  CK4-1102-NM1CHKPTN      PIC  X(01).
      *�ֹ棱ʸ���ܿ��������å�
               05  CK4-1102-NM1SUJI        PIC  X(01).
      *�ֹ棱ʸ���ܴ��������å�
               05  CK4-1102-NM1KANJI       PIC  X(01).
      *�ֹ棱ʸ���ܱѻ������å�
               05  CK4-1102-NM1ABT         PIC  X(01).
      *�ֹ棱ʸ���ܥϥ��ե�Ĺ�������å�
               05  CK4-1102-NM1HYPN        PIC  X(01).
      *�ֹ棱ʸ���ܳ�̥����å�
               05  CK4-1102-NM1KAKKO       PIC  X(01).
      *�ֹ棱ʸ���ܤ���¾�����å�
               05  CK4-1102-NM1ETC         PIC  X(01).
      *�ֹ棲ʸ���ܰʹߥ����å��ѥ�����
               05  CK4-1102-NM2CHKPTN      PIC  X(01).
      *�ֹ棲ʸ���ܰʹ߿��������å�
               05  CK4-1102-NM2SUJI        PIC  X(01).
      *�ֹ棲ʸ���ܰʹߴ��������å�
               05  CK4-1102-NM2KANJI       PIC  X(01).
      *�ֹ棲ʸ���ܰʹ߱ѻ������å�
               05  CK4-1102-NM2ABT         PIC  X(01).
      *�ֹ棲ʸ���ܰʹߥϥ��ե�Ĺ�������å�
               05  CK4-1102-NM2HYPN        PIC  X(01).
      *�ֹ棲ʸ���ܰʹ߳�̥����å�
               05  CK4-1102-NM2KAKKO       PIC  X(01).
      *�ֹ棲ʸ���ܰʹߤ���¾�����å�
               05  CK4-1102-NM2ETC         PIC  X(01).
      *���������å��ѥ��ߡ�
               05  CK4-1102-UPDDUMMY       PIC  X(01).
      *�ֹ�������
               05  CK4-1102-NMKETAMIN      PIC  X(02).
      *�ֹ������
               05  CK4-1102-NMKETAMAX      PIC  X(02).
      *����������
               05  CK4-1102-KGKETAMIN      PIC  X(02).
      *���������
               05  CK4-1102-KGKETAMAX      PIC  X(02).
      *
               05  CK4-1102-YOBI           PIC  X(452).
      *ü���ɣ�
           03  CK4-1102-TERMID             PIC  X(16).
      *���ڡ��졼���ɣ�
           03  CK4-1102-OPID               PIC  X(16).
      *����ǯ����
           03  CK4-1102-CREYMD             PIC  X(08).
      *����ǯ����
           03  CK4-1102-UPYMD              PIC  X(08).
      *��������
           03  CK4-1102-UPHMS              PIC  X(06).