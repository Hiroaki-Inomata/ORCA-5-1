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
      * redistribute this program, but only under the conditions desc
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      ************************************************************
      *    �ݸ��ֹ�ޥ��������ǡ���  (CPHKNNUMWKV02.INC)         *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  HKNWKV02-REC.
      *������ʬ
          03  HKNWKV02-SYORIKBN             PIC X(01).
      *����
          03  HKNWKV02-KEY.
      *���ŵ��أɣ�
               05 HKNWKV02-HOSPID            PIC  X(24).
      *�ݸ��ֹ�
               05 HKNWKV02-HKNNUM            PIC  X(03).
      *Ŭ�ѳ���ǯ����
               05 HKNWKV02-TEKSTYMD          PIC  X(08).
      *��ʧ��ʬ
               05 HKNWKV02-PAYKBN            PIC  X(02).
      *Ŭ�ѽ�λǯ����
           03 HKNWKV02-TEKEDYMD              PIC  X(08).
      *Ŭ�ѽ�λǯ������
           03 HKNWKV02-TEKEDYMD2             PIC  X(08).
      *ˡ���ֹ�
           03 HKNWKV02-HBTNUM                PIC  X(02).
      *����̾
           03 HKNWKV02-SEIDONAME             PIC  X(100).
      *û������̾
           03 HKNWKV02-TANSEIDONAME          PIC  X(20).
      *�ݸ�������̶�ʬ
           03 HKNWKV02-HKNKOHSBTKBN          PIC  X(01).
      *ˡ���ֹ�����å���ʬ
           03 HKNWKV02-HBTNUMCHKKBN          PIC  X(01).
      *�����ֹ�����å���ʬ
           03 HKNWKV02-KENSNUMCHKKBN         PIC  X(01).
      *����Ը����ֹ�����å���ʬ
           03 HKNWKV02-JKYSKENSNUMCHKKBN     PIC  X(01).
      *������ݶ�ʬ
           03 HKNWKV02-HKNKOHKBN             PIC  X(01).
      *�����ݸ��ֹ�
           03 HKNWKV02-GENHKNNUM-G.
      *�����ݸ��ֹ棱
               05 HKNWKV02-GENHKNNUM1        PIC  X(03).
      *�����ݸ��ֹ棲
               05 HKNWKV02-GENHKNNUM2        PIC  X(03).
      *�����ݸ��ֹ棳
               05 HKNWKV02-GENHKNNUM3        PIC  X(03).
      *
           03 HKNWKV02-GENHKNNUM-R REDEFINES   HKNWKV02-GENHKNNUM-G.
               05 HKNWKV02-GENHKNNUM         PIC  X(03)  OCCURS  3.
      *����ǯ�𳫻�
           03 HKNWKV02-JOKEN-STYEAR          PIC  9(03).
      *����ǯ��λ
           03 HKNWKV02-JOKEN-EDYEAR          PIC  9(03).
      *����ñ��
           03 HKNWKV02-TENTNK                PIC  9(03).
      *�쥻�ץ������ʬ
           03 HKNWKV02-RECESKYKBN            PIC  X(01).
      *�쥻�ץȣ���ñ��ɽ����ʬ
           03 HKNWKV02-RECEFTNDSPKBN         PIC  X(01).
      *
           03 HKNWKV02-NAIYOU.
      *�ܿ�
              05 HKNWKV02-HON.
      *  �ܿ͡ݳ������ô��ʬ
                07 HKNWKV02-HON-GAIFTNKBN         PIC  X(01).
      *  �ܿ͡ݳ��������
                07 HKNWKV02-HON-GAI-NAIYO.
      *    �ܿ͡ݳ���ݲ����ô���
                  09 HKNWKV02-HON-GAIKFTNRATE         PIC  9(03).
      *    �ܿ͡ݳ���ݲ�ݸ����
                  09 HKNWKV02-HON-GAIKKOTGAK          PIC  9(06).
      *    �ܿ͡ݳ���ݲ�ݾ�³�
                  09 HKNWKV02-HON-GAIKJGNGAK          PIC  9(06).
      *    �ܿ͡ݳ�������ݾ�³�
                  09 HKNWKV02-HON-GAIDJGNGAK          PIC  9(06).
      *    �ܿ͡ݳ�������ݾ�²��
                  09 HKNWKV02-HON-GAIDJGNCNT          PIC  9(02).
      *    �ܿ͡ݳ���ݷ�ݱ���ݾ�³�
                  09 HKNWKV02-HON-GAIMNAIJGNGAK       PIC  9(06).
      *    �ܿ͡ݳ���ݷ�ݱ����ݾ�³�
                  09 HKNWKV02-HON-GAIMGAIJGNGAK       PIC  9(06).
      *    �ܿ͡ݳ���ݷ�ݾ�²��
                  09 HKNWKV02-HON-GAIMJGNCNT          PIC  9(02).
      *    �ܿ͡ݳ����������ô��ʬ
                  09 HKNWKV02-HON-GAIYKZFTNKBN        PIC  X(01).
      *  �ܿ͡���������ô��ʬ
                07 HKNWKV02-HON-NYUFTNKGN             PIC  X(01).
      *  �ܿ͡�����������
                07 HKNWKV02-HON-NYU-NAIYO.
      *    �ܿ͡������ݲ����ô���
                  09 HKNWKV02-HON-NYUKFTNRATE         PIC  9(03).
      *    �ܿ͡������ݲ�ݸ����
                  09 HKNWKV02-HON-NYUKKOTGAK          PIC  9(06).
      *    �ܿ͡������ݲ�ݾ�³�
                  09 HKNWKV02-HON-NYUKJGNGAK          PIC  9(06).
      *    �ܿ͡����������ݾ�³�
                  09 HKNWKV02-HON-NYUDJGNGAK          PIC  9(06).
      *    �ܿ͡����������ݾ�²��
                  09 HKNWKV02-HON-NYUDGJNCNT          PIC  9(02).
      *    �ܿ͡������ݷ�ݾ�³�
                  09 HKNWKV02-HON-NYUMJGNGAK          PIC  9(06).
      *    �ܿ͡������ݷ�ݾ�²��
                  09 HKNWKV02-HON-NYUMJGNCNT          PIC  9(02).
      *    �ܿ͡������ݿ������ܶ�ʬ
                  09 HKNWKV02-HON-NYUSKJRYYKBN        PIC  X(01).
      *��²
              05 HKNWKV02-KZK.
      *  ��²�ݳ������ô��ʬ
                07 HKNWKV02-KZK-GAIFTNKBN           PIC  X(01).
      *  ��²�ݳ��������
                07 HKNWKV02-KZK-GAI-NAIYO.
      *    ��²�ݳ���ݲ����ô���
                  09 HKNWKV02-KZK-GAIKFTNRATE         PIC  9(03).
      *    ��²�ݳ���ݲ�ݸ����
                  09 HKNWKV02-KZK-GAIKKOTGAK          PIC  9(06).
      *    ��²�ݳ���ݲ�ݾ�³�
                  09 HKNWKV02-KZK-GAIKJGNGAK          PIC  9(06).
      *    ��²�ݳ�������ݾ�³�
                  09 HKNWKV02-KZK-GAIDJGNGAK          PIC  9(06).
      *    ��²�ݳ�������ݾ�²��
                  09 HKNWKV02-KZK-GAIDJGNCNT          PIC  9(02).
      *    ��²�ݳ���ݷ�ݱ���ݾ�³�
                  09 HKNWKV02-KZK-GAIMNAIJGNGAK       PIC  9(06).
      *    ��²�ݳ���ݷ�ݱ����ݾ�³�
                  09 HKNWKV02-KZK-GAIMGAIJGNGAK       PIC  9(06).
      *    ��²�ݳ���ݷ�ݾ�²��
                  09 HKNWKV02-KZK-GAIMJGNCNT          PIC  9(02).
      *    ��²�ݳ����������ô��ʬ
                  09 HKNWKV02-KZK-GAIYKZFTNKBN        PIC  X(01).
      *  ��²����������ô��ʬ
                07 HKNWKV02-KZK-NYUFTNKBN             PIC  X(01).
      *  ��²������������
                07 HKNWKV02-KZK-NYU-NAIYO.
      *    ��²�������ݲ����ô���
                  09 HKNWKV02-KZK-NYUKFTNRATE         PIC  9(03).
      *    ��²�������ݲ�ݸ����
                  09 HKNWKV02-KZK-NYUKKOTGAK          PIC  9(06).
      *    ��²�������ݲ�ݾ�³�
                  09 HKNWKV02-KZK-NYUKJGNGAK          PIC  9(06).
      *    ��²�����������ݾ�³�
                  09 HKNWKV02-KZK-NYUDJGNGAK          PIC  9(06).
      *    ��²�����������ݾ�²��
                  09 HKNWKV02-KZK-NYUDJGNCNT          PIC  9(02).
      *    ��²�������ݷ�ݾ�³�
                  09 HKNWKV02-KZK-NYUMJGNGAK          PIC  9(06).
      *    ��²�������ݷ�ݾ�²��
                  09 HKNWKV02-KZK-NYUMJGNCNT          PIC  9(02).
      *    ��²�������ݿ������ܶ�ʬ
                  09 HKNWKV02-KZK-NYUSKJRYYKBN        PIC  X(01).
      *�����
              05 HKNWKV02-TSY.
      *  ������ݳ������ô��ʬ
                07 HKNWKV02-TSY-GAIFTNKBN         PIC  X(01).
      *  ������ݳ��������
                07 HKNWKV02-TSY-GAI-NAIYO.
      *    ������ݳ���ݲ����ô���
                  09 HKNWKV02-TSY-GAIKFTNRATE      PIC  9(03).
      *    ������ݳ���ݲ�ݸ����
                  09 HKNWKV02-TSY-GAIKKOTGAK       PIC  9(06).
      *    ������ݳ���ݲ�ݾ�³�
                  09 HKNWKV02-TSY-GAIKJGNGAK       PIC  9(06).
      *    ������ݳ�������ݾ�³�
                  09 HKNWKV02-TSY-GAIDJGNGAK       PIC  9(06).
      *    ������ݳ�������ݾ�²��
                  09 HKNWKV02-TSY-GAIDJGNCNT       PIC  9(02).
      *    ������ݳ���ݷ�ݱ���ݾ�³�
                  09 HKNWKV02-TSY-GAIMNAIJGNGAK    PIC  9(06).
      *    ������ݳ���ݷ�ݱ����ݾ�³�
                  09 HKNWKV02-TSY-GAIMGAIJGNGAK    PIC  9(06).
      *    ������ݳ���ݷ�ݾ�²��
                  09 HKNWKV02-TSY-GAIMJGNCNT       PIC  9(02).
      *    ������ݳ����������ô��ʬ
                  09 HKNWKV02-TSY-GAIYKZFTNKBN     PIC  X(01).
      *  ���������������ô��ʬ
                07 HKNWKV02-TSY-NYUFTNNBK          PIC  X(01).
      *  �����������������
                07 HKNWKV02-TSY-NYU-NAIYO.
      *    ������������ݲ����ô���
                  09 HKNWKV02-TSY-NYUKFTNRATE      PIC  9(03).
      *    ������������ݲ�ݸ����
                  09 HKNWKV02-TSY-NYUKKOTGAK       PIC  9(06).
      *    ������������ݲ�ݾ�³�
                  09 HKNWKV02-TSY-NYUKJGNGAK       PIC  9(06).
      *    ����������������ݾ�³�
                  09 HKNWKV02-TSY-NYUDJGNGAK       PIC  9(06).
      *    ����������������ݾ�²��
                  09 HKNWKV02-TSY-NYUDJGNCNT       PIC  9(02).
      *    ������������ݷ�ݾ�³�
                  09 HKNWKV02-TSY-NYUMJGNGAK       PIC  9(06).
      *    ������������ݷ�ݾ�²��
                  09 HKNWKV02-TSY-NYUMJGNCNT       PIC  9(02).
      *    ������������ݿ������ܶ�ʬ
                  09 HKNWKV02-TSY-NYUSKJRYYKBN     PIC  X(01).
      *��ǯ��
              05 HKNWKV02-TNK.
      *  ��ǯ��ݳ������ô��ʬ
                07 HKNWKV02-TNK-GAIFTNKBN         PIC  X(01).
      *  ��ǯ��ݳ���������
                07 HKNWKV02-TNK-GAI-NAIYO.
      *    ��ǯ��ݳ���ݲ����ô���
                  09 HKNWKV02-TNK-GAIKFTNRATE      PIC  9(03).
      *    ��ǯ��ݳ���ݲ�ݸ����
                  09 HKNWKV02-TNK-GAIKKOTGAK       PIC  9(06).
      *    ��ǯ��ݳ���ݲ�ݾ�³�
                  09 HKNWKV02-TNK-GAIKJGNGAK       PIC  9(06).
      *    ��ǯ��ݳ�������ݾ�³�
                  09 HKNWKV02-TNK-GAIDJGNGAK       PIC  9(06).
      *    ��ǯ��ݳ�������ݾ�²��
                  09 HKNWKV02-TNK-GAIDJGNCNT       PIC  9(02).
      *    ��ǯ��ݳ���ݷ�ݱ���ݾ�³�
                  09 HKNWKV02-TNK-GAIMNAIJGNGAK    PIC  9(06).
      *    ��ǯ��ݳ���ݷ�ݱ����ݾ�³�
                  09 HKNWKV02-TNK-GAIMGAIJGNGAK    PIC  9(06).
      *    ��ǯ��ݳ���ݷ�ݾ�²��
                  09 HKNWKV02-TNK-GAIMJGNCNT       PIC  9(02).
      *    ��ǯ��ݳ����������ô��ʬ
                  09 HKNWKV02-TNK-GAIYKZFTNKBN     PIC  X(01).
      *  ��ǯ�����������ô��ʬ
                07 HKNWKV02-TNK-NYUFTNKBN        PIC  X(01).
      *  ��ǯ�������������
                07 HKNWKV02-TNK-NYU-NAIYO.
      *    ��ǯ��������ݲ����ô���
                  09 HKNWKV02-TNK-NYUKFTNRATE      PIC  9(03).
      *    ��ǯ��������ݲ�ݸ����
                  09 HKNWKV02-TNK-NYUKKOTGAK       PIC  9(06).
      *    ��ǯ��������ݲ�ݾ�³�
                  09 HKNWKV02-TNK-NYUKJGNGAK       PIC  9(06).
      *    ��ǯ������������ݾ�³�
                  09 HKNWKV02-TNK-NYUDJGNGAK       PIC  9(06).
      *    ��ǯ������������ݾ�²��
                  09 HKNWKV02-TNK-NYUDJGNCNT       PIC  9(02).
      *    ��ǯ��������ݷ�ݾ�³�
                  09 HKNWKV02-TNK-NYUMJGNGAK       PIC  9(06).
      *    ��ǯ��������ݷ�ݾ�²��
                  09 HKNWKV02-TNK-NYUMJGNCNT       PIC  9(02).
      *    ��ǯ��������ݿ������ܶ�ʬ
                  09 HKNWKV02-TNK-NYUSKJRYYKBN     PIC  X(01).
      *
           03 HKNWKV02-NAIYOU-R REDEFINES   HKNWKV02-NAIYOU.
              05 HKNWKV02-TBL                  OCCURS  4.
      *  �������ô��ʬ
                07 HKNWKV02-GAIFTNKBN          PIC  X(01).
      *  ���������
                07 HKNWKV02-GAI-NAIYO.
      *    ����ݲ����ô���
                  09 HKNWKV02-GAIKFTNRATE         PIC  9(03).
      *    ����ݲ�ݸ����
                  09 HKNWKV02-GAIKKOTGAK          PIC  9(06).
      *    ����ݲ�ݾ�³�
                  09 HKNWKV02-GAIKJGNGAK          PIC  9(06).
      *    ��������ݾ�³�
                  09 HKNWKV02-GAIDJGNGAK          PIC  9(06).
      *    ��������ݾ�²��
                  09 HKNWKV02-GAIDJGNCNT          PIC  9(02).
      *    ����ݷ�ݱ���ݾ�³�
                  09 HKNWKV02-GAIMNAIJGNGAK       PIC  9(06).
      *    ����ݷ�ݱ����ݾ�³�
                  09 HKNWKV02-GAIMGAIJGNGAK       PIC  9(06).
      *    ����ݷ�ݾ�²��
                  09 HKNWKV02-GAIMJGNCNT          PIC  9(02).
      *    �����������ô��ʬ
                  09 HKNWKV02-GAIYKZFTNKBN        PIC  X(01).
      *  ��������ô��ʬ
                07 HKNWKV02-NYUFTNKBN             PIC  X(01).
      *  ����������
                07 HKNWKV02-NYU-NAIYO.
      *    �����ݲ����ô���
                  09 HKNWKV02-NYUKFTNRATE         PIC  9(03).
      *    �����ݲ�ݸ����
                  09 HKNWKV02-NYUKKOTGAK          PIC  9(06).
      *    �����ݲ�ݾ�³�
                  09 HKNWKV02-NYUKJGNGAK          PIC  9(06).
      *    ���������ݾ�³�
                  09 HKNWKV02-NYUDJGNGAK          PIC  9(06).
      *    ���������ݾ�²��
                  09 HKNWKV02-NYUDJGNCNT          PIC  9(02).
      *    �����ݷ�ݾ�³�
                  09 HKNWKV02-NYUMJGNGAK          PIC  9(06).
      *    �����ݷ�ݾ�²��
                  09 HKNWKV02-NYUMJGNCNT          PIC  9(02).
      *    �����ݿ������ܶ�ʬ
                  09 HKNWKV02-NYUSKJRYYKBN        PIC  X(01).
      *����ü���ɣ�
           03 HKNWKV02-TERMID                  PIC  X(16).
      *�������ڡ��졼���ɣ�
           03 HKNWKV02-OPID                    PIC  X(16).
      *��������ǯ����
           03 HKNWKV02-CREYMD                  PIC  X(08).
      *��������ǯ����
           03 HKNWKV02-UPYMD                   PIC  X(08).
      *������������
           03 HKNWKV02-UPHMS                   PIC  X(06).
