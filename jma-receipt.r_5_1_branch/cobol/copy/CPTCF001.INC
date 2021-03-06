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
      *    ����ץǡ����ʴ��ܾ����     COPY (CPTCF001.INC)    *
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *
      *  04.02.01    NACL-ƣ��    08/04/10  ʿ������ǯ��������б�
      *                                     ��������ʬ�ɲ�
      *
      *  04.05.01    NACL-ƣ��    12/07/02  �����������������ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *  04.06.02    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      ************************************************************
      *01  TOUKEI01-REC.
           03  TOUKEI01-KEY.
      *        ���ŵ��أɣ�
               05  TOUKEI01-HOSPNUM        PIC 9(2).
      *        ����ǯ��
               05  TOUKEI01-SRYYM          PIC  9(06).
      *        ������ʬ
               05  TOUKEI01-NYUGAIKBN      PIC  X(01).
      *        ���ԣɣ�
               05  TOUKEI01-PTID           PIC  9(10).
      *        �������ʬ
               05  TOUKEI01-RECEIPT-KBN    PIC  X(10).
      *        �쥻�ץȿ��Ų�
               05  TOUKEI01-RECEKA         PIC  X(02).
      *        ���ݸ��ݸ��ֹ�
               05  TOUKEI01-HKNNUM         PIC  X(03).
      *        ����ɣ�
               05  TOUKEI01-KOHID          PIC  9(10).
      *        �ݸ����ֹ�        
               05  TOUKEI01-HKNJANUM       PIC  X(08).
      *        �ܿͲ�²��ʬ
               05  TOUKEI01-HONKZKKBN      PIC  X(01).
      *        �����ʬ
               05  TOUKEI01-HOJOKBN        PIC  X(01).
      *        ����ɣġʸ���ñ�Ȼ����ѡ�
               05  TOUKEI01-KOH-KOHID      PIC  9(10).
      *
      *        ���ݸ��ɣ�
           03  TOUKEI01-HKNID              PIC  9(10).
      *
      *    ���ݸ�����
           03  TOUKEI01-HKN-AREA.
               05  TOUKEI01-HKN-TBL        OCCURS  5.
      *            ���ݸ����ݸ��ɣ�
                   07  TOUKEI01-THKNID     PIC  9(10).
      *            ���ݸ����ݸ����ֹ�
      *****        07  TOUKEI01-THKNJANUM  PIC  X(08).
      *            ���ݸ��ݵ���
                   07  TOUKEI01-TKIGO      PIC  X(80).
      *            ���ݸ����ֹ�
                   07  TOUKEI01-TNUM       PIC  X(80).
      *            ���ݸ���̾
                   07  TOUKEI01-THIHKNJANAME
                                           PIC  X(100).
      *            ��ʼ���ǯ����
                   07  TOUKEI01-TSKKGETYMD PIC  X(08).
      *            Ŭ�ѳ���ǯ����
                   07  TOUKEI01-TTEKSTYMD  PIC  X(08). 
      *            Ŭ�ѽ�λǯ����
                   07  TOUKEI01-TTEKEDYMD  PIC  X(08). 
      *            ���ݸ��ݷ�³��ʬ
                   07  TOUKEI01-TCONTKBN   PIC  X(01).
      *            ���ݸ��������ʬ
                   07  TOUKEI01-THOJOKBN   PIC X(01).
      *            ���ݸ����ܿͲ�²��ʬ
                   07  TOUKEI01-THONKZKKBN PIC  X(01).
      *        ���ݸ���ˡ���ֹ�
               05  TOUKEI01-HKNHBTNUM      PIC  X(02).
      *        ���ݸ��ݸ�����̶�ʬ
               05  TOUKEI01-HKNKOHSBTKBN   PIC X(01).
      *
      *    Ϸ���ݷ����
           03  TOUKEI01-RJNHKN.
      *        Ϸ���ݸ����ݸ��ֹ�
               05  TOUKEI01-RJNHKNNUM      PIC  X(03).
      *        Ϸ�ͻ�ʧ��ʬ
               05  TOUKEI01-RJNPAYKBN      PIC  X(02).
      *        Ϸ���ݸ���ˡ���ֹ�
               05  TOUKEI01-RJNHBTNUM      PIC  X(02).
      *        Ϸ���ݸ��ݻ�Į¼�ֹ�
               05  TOUKEI01-RJNFTNJANUM    PIC  X(08).
      *        Ϸ���ݸ��ݼ�����ֹ�
               05  TOUKEI01-RJNJKYSNUM     PIC  X(07).
      *        Ϸ��Ŭ�ѳ���ǯ����
               05  TOUKEI01-RJNTEKSTYMD    PIC  X(08).
      *        Ϸ��Ŭ�ѽ�λǯ����
               05  TOUKEI01-RJNTEKEDYMD    PIC  X(08).
      * 
      *    �������
           03 TOUKEI01-KOHI-AREA.
               05  TOUKEI01-KOHI-TBL       OCCURS  10.
      *            ����ɣ�
                   07  TOUKEI01-TKOHID     PIC  9(10).
      *            �����ݸ��ֹ�
                   07  TOUKEI01-TKOHHKNNUM PIC  X(03).
      *            �����ʧ��ʬ
                   07  TOUKEI01-TKOHPAYKBN PIC  X(02).
      *            �����ˡ���ֹ�
                   07  TOUKEI01-TKOHHBTNUM PIC  X(02).
      *            �������ô���ֹ�
                   07  TOUKEI01-TKOHFTNJANUM
                                           PIC  X(08).
      *            ����ݼ�����ֹ�
                   07  TOUKEI01-TKOHJKYSNUM
                                           PIC  X(20).
      *            ����ݥ쥻�ץ������ʬ
                   07  TOUKEI01-TRECESKYKBN PIC  X(01).
      *            Ŭ�ѳ���ǯ����
                   07  TOUKEI01-TKOHTEKSTYMD    PIC  X(08).
      *            Ŭ�ѽ�λǯ����
                   07  TOUKEI01-TKOHTEKEDYMD    PIC  X(08).
      *
      *    ϫ�Ҿ���
           03  TOUKEI01-HKN.
      *        ϫ�ҡ�ϫ���ݸ���ʬ
               05  TOUKEI01-HKNKBN          PIC  X(01).
      *        ϫ�ҡݴ��Ը����ֹ�
               05  TOUKEI01-KOFUNUM         PIC  X(14).
      *        ϫ�ҡ����ݸ���̾
               05  TOUKEI01-HIHKNJANAME     PIC  X(100).
      *        ϫ�ҡݻ��Ƚ�̾��
               05  TOUKEI01-JIGYOUNAME      PIC  X(80).
      *        ϫ�ҡݽ������ƻ�ܸ�̾
               05  TOUKEI01-PREFNAME        PIC  X(20).
      *        ϫ�ҡݽ������ƻ�ܸ���ʬ
               05  TOUKEI01-PREFKBN         PIC  X(01).
      *        ϫ�ҡݽ���Ϸ��Զ�̾
               05  TOUKEI01-CITYNAME        PIC  X(80).
      *        ϫ�ҡݽ���Ϸ��Զ��ʬ
               05  TOUKEI01-CITYKBN         PIC  X(01).
      *        ϫ�ҡݿ��Ѻ���
               05  TOUKEI01-SINKEI          PIC  X(01).
      *        ϫ�ҡ�ž����ͳ
               05  TOUKEI01-TENKI           PIC  X(01).
      *        ϫ�ҡݽ���ǯ����
               05  TOUKEI01-SHOBYOYMD       PIC  X(08).
      *        ϫ�ҡݻͻ������ʬ
               05  TOUKEI01-SISIKBN         PIC  X(01).
      *        ϫ�ҡ����ܳ���ǯ����
               05  TOUKEI01-RYOSTYMD        PIC  X(08).
      *        ϫ�ҡ����ܽ�λǯ����
               05  TOUKEI01-RYOEDYMD        PIC  X(08).
      *        ϫ�ҡ�»����ʬ
               05  TOUKEI01-SONSHOUKBN      PIC  X(02).
      *        ϫ�ҡ�»����ʬ�ݻ���
               05  TOUKEI01-SONSHOUKBN-EDANUM
                                            PIC  X(01).
      *        ϫ�ҡݥ��ե������ܳ���ǯ����
               05  TOUKEI01-SINSATUYMD      PIC  X(08).
      *        ϫ�ҡݥ��ե������ܽ�λǯ����
               05  TOUKEI01-SINSATUEND      PIC  X(08).
      *        ϫ�ҡݺҳ���ʬ
               05  TOUKEI01-SAIGAIKBN       PIC  X(01).
      *        ϫ�ҡݼ����������ʬ
               05  TOUKEI01-JIBAISEIKBN     PIC  X(01).
      *
      *    ���Ծ���
           03  TOUKEI01-PTINF-AREA.
      *        �����ֹ�  
               05  TOUKEI01-PTNUM           PIC  X(20).
      *        ���ʻ�̾
               05  TOUKEI01-KANANAME        PIC  X(50).
      *        ������̾
               05  TOUKEI01-NAME            PIC  X(100).
      *        ����
               05  TOUKEI01-SEX             PIC  X(01).
      *        ��ǯ����
               05  TOUKEI01-BIRTHDAY        PIC  9(08).
      *        ǯ��
               05  TOUKEI01-AGE             PIC  9(03).
      *        ��������ʬ�ʣ�����������
               05  TOUKEI01-SYUGAKUKBN      PIC 9(01).
      *        �����͹���ֹ�
               05  TOUKEI01-HOME-POST       PIC  X(07).
      *        ����ݽ���
               05  TOUKEI01-HOME-ADRS       PIC  X(400).
      *        ������������ֹ�
               05  TOUKEI01-HOME-TEL1       PIC  X(15).
      *       �����������Υ�����
               05  TOUKEI01-LPUBCD          PIC  X(05).
      *    �ݸ��ȹ礻
           03 TOUKEI01-HKNCOMBIINF.
               05  TOUKEI01-HKNCOMBI        PIC  9(04)  OCCURS  10.
      *    ���Ųʾ���
           03 TOUKEI01-SRYKAINF.
               05  TOUKEI01-SRYKA           PIC  X(02)  OCCURS  20.
      *
      *    �쥻�ץȼ���
           03  TOUKEI01-RECESYUBETU         PIC  9(04).
      *
      *    ̤������ʬ
           03  TOUKEI01-TEISEIKBN           PIC  X(01).
