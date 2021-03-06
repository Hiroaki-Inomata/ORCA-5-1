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
      *    ����Ԥΰ�����ô��۵��ܤ˴ؤ�������å��ѥ�᥿      *
      *                         COPY  (CPORCSKOREIFTNCHK.INC)    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  03.02.01    NACL-����    06/08/10  ����Էв����֥ե饰�ɲ�
      *                                     ���ݸ�ñ�Ȥδ�����ô���ɲ�
      *                                     �����ޤ���δ�����ô���ɲ�
      *  03.03.01    NACL-����    06/11/02  ���ۡ��Ƚ�����ʧͱͽ�б�
      *  03.04.01    NACL-����    06/11/20  ��ۣ����ܰʹ��б�
      *  03.04.02    NACL-����    07/03/16  �������ͭ��̵���ե饰�ɲ�
      *                                     �����ޤ���μ��ݸ��ΰ�����
      *                                     ô�ۣ��ɲ�
      *  03.04.03    NACL-����    07/04/01  ����ԡ���Ͷ�ʬ�ɲ�
      *                                     ǯ���ɲ�
      *                                     ��Ͷ�ʬ�ɲ�
      *                                     �ֵ��ͤ��������ɲ�
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *  03.05.02    NACL-����    07/12/03  ���ֹ��ɲ�
      *  04.02.01    NACL-����    08/04/01  ��������ʬ�ɲ�
      *                                     �ڸ����������оݼԶ�ʬ�ɲ�
      *                                     �ޥ�Ĺ����ɣ��ɲ�
      *  04.03.01    NACL-����    09/01/05  ��������ã��������ʬ�ɲ�
      *                                     �ޥ�Ĺ�Τ߶�ʬ�ɲ�
      *  04.07.01    NACL-����    13/11/11  ���ۡʱߡˤ��б�
      ************************************************************
       01  ORCSKOREIFTNCHKAREA.
      *
      *������
      *    ���ŵ��أɣ�
           03  LNK-KOREIFTNCHK-HOSPNUM      PIC 9(2).
      *    ���ԣɣ�
           03  LNK-KOREIFTNCHK-PTID         PIC 9(10).
      *    ������ʬ
           03  LNK-KOREIFTNCHK-NYUGAIKBN    PIC X(01).
      *    ����ǯ��
           03  LNK-KOREIFTNCHK-SRYYM        PIC X(06).
      *    ���ݸ����ݸ��ֹ�
           03  LNK-KOREIFTNCHK-HKNNUM       PIC X(03).
      *    ���ݸ����ݸ��ɣ�
           03  LNK-KOREIFTNCHK-HKNID        PIC 9(10).
      *    ������ե饰
           03  LNK-KOREIFTNCHK-TEISYOTOKU   PIC X(01).
      *    �쥻�ץȲ�
           03  LNK-KOREIFTNCHK-RECEKA       PIC X(02).
      *    ����Էв����֥ե饰
           03  LNK-KOREIFTNCHK-KEIKASOCHI   PIC X(01).
      *    ���۶�ʬ
           03  LNK-KOREIFTNCHK-GENGAKUKBN   PIC 9(01).
      *    ���ۡʳ���
           03  LNK-KOREIFTNCHK-GENGAKU-RITU PIC 9(03).
      *    ���ۡʶ�ۡ�
           03  LNK-KOREIFTNCHK-GENGAKU-KIN  PIC 9(07).
      *    ���۷׻��ե饰
           03  LNK-KOREIFTNCHK-GENGAKUFLG   PIC 9(01).
      *    ��ۣ����ܰʹ�
           03  LNK-KOREIFTNCHK-KOHYONKAIME  PIC 9(01).
      *    ����ԡ���Ͷ�ʬ
           03  LNK-KOREIFTNCHK-NENREIKBN    PIC X(01).
      *    ǯ��
           03  LNK-KOREIFTNCHK-AGE          PIC 9(03).
      *    ���ֹ�
           03  LNK-KOREIFTNCHK-PREFNUM      PIC X(02).
      *    ��������ʬ
           03  LNK-KOREIFTNCHK-SYUGAKUKBN   PIC 9(01).
      *    �ޥ�Ĺ����ɣ�
           03  LNK-KOREIFTNCHK-KOHID-CYOKI  PIC 9(10).
      *    ��������ã��������ʬ
           03  LNK-KOREIFTNCHK-TOKU75       PIC 9(01).
      *    �ޥ�Ĺ�Τ߶�ʬ
           03  LNK-KOREIFTNCHK-CYOKINOMI    PIC 9(01).
      *    ������ʬ
           03  LNK-KOREIFTNCHK-SYOTOKUKBN   PIC 9(01).
      *
      *�ֵ���
           03  LNK-KOREIFTNCHK-OUT-AREA.
      *���ݸ�ñ�Ȥδ�����ô��
             05 LNK-KOREIFTNCHK-FTNGAKU     PIC 9(07).
      *�����ޤ���δ�����ô��
             05 LNK-KOREIFTNCHK-KOHFTNGAKU  PIC 9(07).
      *�����ޤ���μ��ݸ��ΰ�����ô��
             05 LNK-KOREIFTNCHK-ICHIBUGAKU  PIC 9(07).
      *�����ޤ���μ��ݸ��ΰ�����ô�ۡʣ���ñ�̡ˡ�Ƚ���ѡ�
             05 LNK-KOREIFTNCHK-ICHIBUGAKU2 PIC 9(07).
      *����������ԡ����������������
             05 LNK-KOREIFTNCHK-CHIHOUGAKU  PIC 9(07).
      *
      *����������
           03  LNK-KOREIFTNCHK-RC           PIC 9(02).
