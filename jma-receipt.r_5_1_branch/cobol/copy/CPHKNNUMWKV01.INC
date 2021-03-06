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
      *    �ݸ��ֹ�ޥ��������ǡ���  (CPHKNNUMWKV01.INC)         *
      *    �ʵ�쥤�����ȡ�
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  HKNWKV01-REC.
      *������ʬ
          03  HKNWKV01-SYORIKBN             PIC X(01).
      *����
          03  HKNWKV01-KEY.
      *���ŵ��أɣ�
               05 HKNWKV01-HOSPID            PIC  X(24).
      *�ݸ��ֹ�
               05 HKNWKV01-HKNNUM            PIC  X(03).
      *Ŭ�ѳ���ǯ����
               05 HKNWKV01-TEKSTYMD          PIC  X(08).
      *��ʧ��ʬ
               05 HKNWKV01-PAYKBN            PIC  X(02).
      *Ŭ�ѽ�λǯ����
           03 HKNWKV01-TEKEDYMD              PIC  X(08).
      *Ŭ�ѽ�λǯ������
           03 HKNWKV01-TEKEDYMD2             PIC  X(08).
      *ˡ���ֹ�
           03 HKNWKV01-HBTNUM                PIC  X(02).
      *����̾
           03 HKNWKV01-SEIDONAME             PIC  X(100).
      *û������̾
           03 HKNWKV01-TANSEIDONAME          PIC  X(20).
      *�ݸ�������̶�ʬ
           03 HKNWKV01-HKNKOHSBTKBN          PIC  X(01).
      *�����ֹ�����å���ʬ
           03 HKNWKV01-KENSNUMCHKKBN         PIC  X(01).
      *����Ը����ֹ�����å���ʬ
           03 HKNWKV01-JKYSKENSNUMCHKKBN     PIC  X(01).
      *������ݶ�ʬ
           03 HKNWKV01-HKNKOHKBN             PIC  X(01).
      *�����ݸ��ֹ�
           03 HKNWKV01-GENHKNNUM-G.
      *�����ݸ��ֹ棱
               05 HKNWKV01-GENHKNNUM1        PIC  X(03).
      *�����ݸ��ֹ棲
               05 HKNWKV01-GENHKNNUM2        PIC  X(03).
      *�����ݸ��ֹ棳
               05 HKNWKV01-GENHKNNUM3        PIC  X(03).
      *
           03 HKNWKV01-GENHKNNUM-R REDEFINES   HKNWKV01-GENHKNNUM-G.
               05 HKNWKV01-GENHKNNUM         PIC  X(03)  OCCURS  3.
      *����ǯ�𳫻�
           03 HKNWKV01-JOKEN-STYEAR          PIC  9(03).
      *����ǯ��λ
           03 HKNWKV01-JOKEN-EDYEAR          PIC  9(03).
      *����ñ��
           03 HKNWKV01-TENTNK                PIC  9(03).
      *�쥻�ץ������ʬ
           03 HKNWKV01-RECESKYKBN            PIC  X(01).
      *
           03 HKNWKV01-NAIYOU.
      *�ܿ�
              05 HKNWKV01-HON.
      *  �ܿ͡ݳ������ô��ʬ
                07 HKNWKV01-HON-GAIFTNKBN         PIC  X(01).
      *  �ܿ͡ݳ��������
                07 HKNWKV01-HON-GAI-NAIYO.
      *    �ܿ͡ݳ���ݲ����ô���
                  09 HKNWKV01-HON-GAIKFTNRATE         PIC  9(03).
      *    �ܿ͡ݳ���ݲ�ݸ����
                  09 HKNWKV01-HON-GAIKKOTGAK          PIC  9(06).
      *    �ܿ͡ݳ���ݲ�ݾ�³�
                  09 HKNWKV01-HON-GAIKJGNGAK          PIC  9(06).
      *    �ܿ͡ݳ�������ݾ�³�
                  09 HKNWKV01-HON-GAIDJGNGAK          PIC  9(06).
      *    �ܿ͡ݳ�������ݾ�²��
                  09 HKNWKV01-HON-GAIDJGNCNT          PIC  9(02).
      *    �ܿ͡ݳ���ݷ�ݱ���ݾ�³�
                  09 HKNWKV01-HON-GAIMNAIJGNGAK       PIC  9(06).
      *    �ܿ͡ݳ���ݷ�ݱ����ݾ�³�
                  09 HKNWKV01-HON-GAIMGAIJGNGAK       PIC  9(06).
      *    �ܿ͡ݳ���ݷ�ݾ�²��
                  09 HKNWKV01-HON-GAIMJGNCNT          PIC  9(02).
      *    �ܿ͡ݳ����������ô��ʬ
                  09 HKNWKV01-HON-GAIYKZFTNKBN        PIC  X(01).
      *  �ܿ͡���������ô��ʬ
                07 HKNWKV01-HON-NYUFTNKGN             PIC  X(01).
      *  �ܿ͡�����������
                07 HKNWKV01-HON-NYU-NAIYO.
      *    �ܿ͡������ݲ����ô���
                  09 HKNWKV01-HON-NYUKFTNRATE         PIC  9(03).
      *    �ܿ͡������ݲ�ݸ����
                  09 HKNWKV01-HON-NYUKKOTGAK          PIC  9(06).
      *    �ܿ͡������ݲ�ݾ�³�
                  09 HKNWKV01-HON-NYUKJGNGAK          PIC  9(06).
      *    �ܿ͡����������ݾ�³�
                  09 HKNWKV01-HON-NYUDJGNGAK          PIC  9(06).
      *    �ܿ͡����������ݾ�²��
                  09 HKNWKV01-HON-NYUDGJNCNT          PIC  9(02).
      *    �ܿ͡������ݷ�ݾ�³�
                  09 HKNWKV01-HON-NYUMJGNGAK          PIC  9(06).
      *    �ܿ͡������ݷ�ݾ�²��
                  09 HKNWKV01-HON-NYUMJGNCNT          PIC  9(02).
      *    �ܿ͡������ݿ������ܶ�ʬ
                  09 HKNWKV01-HON-NYUSKJRYYKBN        PIC  X(01).
      *��²
              05 HKNWKV01-KZK.
      *  ��²�ݳ������ô��ʬ
                07 HKNWKV01-KZK-GAIFTNKBN           PIC  X(01).
      *  ��²�ݳ��������
                07 HKNWKV01-KZK-GAI-NAIYO.
      *    ��²�ݳ���ݲ����ô���
                  09 HKNWKV01-KZK-GAIKFTNRATE         PIC  9(03).
      *    ��²�ݳ���ݲ�ݸ����
                  09 HKNWKV01-KZK-GAIKKOTGAK          PIC  9(06).
      *    ��²�ݳ���ݲ�ݾ�³�
                  09 HKNWKV01-KZK-GAIKJGNGAK          PIC  9(06).
      *    ��²�ݳ�������ݾ�³�
                  09 HKNWKV01-KZK-GAIDJGNGAK          PIC  9(06).
      *    ��²�ݳ�������ݾ�²��
                  09 HKNWKV01-KZK-GAIDJGNCNT          PIC  9(02).
      *    ��²�ݳ���ݷ�ݱ���ݾ�³�
                  09 HKNWKV01-KZK-GAIMNAIJGNGAK       PIC  9(06).
      *    ��²�ݳ���ݷ�ݱ����ݾ�³�
                  09 HKNWKV01-KZK-GAIMGAIJGNGAK       PIC  9(06).
      *    ��²�ݳ���ݷ�ݾ�²��
                  09 HKNWKV01-KZK-GAIMJGNCNT          PIC  9(02).
      *    ��²�ݳ����������ô��ʬ
                  09 HKNWKV01-KZK-GAIYKZFTNKBN        PIC  X(01).
      *  ��²����������ô��ʬ
                07 HKNWKV01-KZK-NYUFTNKBN             PIC  X(01).
      *  ��²������������
                07 HKNWKV01-KZK-NYU-NAIYO.
      *    ��²�������ݲ����ô���
                  09 HKNWKV01-KZK-NYUKFTNRATE         PIC  9(03).
      *    ��²�������ݲ�ݸ����
                  09 HKNWKV01-KZK-NYUKKOTGAK          PIC  9(06).
      *    ��²�������ݲ�ݾ�³�
                  09 HKNWKV01-KZK-NYUKJGNGAK          PIC  9(06).
      *    ��²�����������ݾ�³�
                  09 HKNWKV01-KZK-NYUDJGNGAK          PIC  9(06).
      *    ��²�����������ݾ�²��
                  09 HKNWKV01-KZK-NYUDJGNCNT          PIC  9(02).
      *    ��²�������ݷ�ݾ�³�
                  09 HKNWKV01-KZK-NYUMJGNGAK          PIC  9(06).
      *    ��²�������ݷ�ݾ�²��
                  09 HKNWKV01-KZK-NYUMJGNCNT          PIC  9(02).
      *    ��²�������ݿ������ܶ�ʬ
                  09 HKNWKV01-KZK-NYUSKJRYYKBN        PIC  X(01).
      *�����
              05 HKNWKV01-TSY.
      *  ������ݳ������ô��ʬ
                07 HKNWKV01-TSY-GAIFTNKBN         PIC  X(01).
      *  ������ݳ��������
                07 HKNWKV01-TSY-GAI-NAIYO.
      *    ������ݳ���ݲ����ô���
                  09 HKNWKV01-TSY-GAIKFTNRATE      PIC  9(03).
      *    ������ݳ���ݲ�ݸ����
                  09 HKNWKV01-TSY-GAIKKOTGAK       PIC  9(06).
      *    ������ݳ���ݲ�ݾ�³�
                  09 HKNWKV01-TSY-GAIKJGNGAK       PIC  9(06).
      *    ������ݳ�������ݾ�³�
                  09 HKNWKV01-TSY-GAIDJGNGAK       PIC  9(06).
      *    ������ݳ�������ݾ�²��
                  09 HKNWKV01-TSY-GAIDJGNCNT       PIC  9(02).
      *    ������ݳ���ݷ�ݱ���ݾ�³�
                  09 HKNWKV01-TSY-GAIMNAIJGNGAK    PIC  9(06).
      *    ������ݳ���ݷ�ݱ����ݾ�³�
                  09 HKNWKV01-TSY-GAIMGAIJGNGAK    PIC  9(06).
      *    ������ݳ���ݷ�ݾ�²��
                  09 HKNWKV01-TSY-GAIMJGNCNT       PIC  9(02).
      *    ������ݳ����������ô��ʬ
                  09 HKNWKV01-TSY-GAIYKZFTNKBN     PIC  X(01).
      *  ���������������ô��ʬ
                07 HKNWKV01-TSY-NYUFTNNBK          PIC  X(01).
      *  �����������������
                07 HKNWKV01-TSY-NYU-NAIYO.
      *    ������������ݲ����ô���
                  09 HKNWKV01-TSY-NYUKFTNRATE      PIC  9(03).
      *    ������������ݲ�ݸ����
                  09 HKNWKV01-TSY-NYUKKOTGAK       PIC  9(06).
      *    ������������ݲ�ݾ�³�
                  09 HKNWKV01-TSY-NYUKJGNGAK       PIC  9(06).
      *    ����������������ݾ�³�
                  09 HKNWKV01-TSY-NYUDJGNGAK       PIC  9(06).
      *    ����������������ݾ�²��
                  09 HKNWKV01-TSY-NYUDJGNCNT       PIC  9(02).
      *    ������������ݷ�ݾ�³�
                  09 HKNWKV01-TSY-NYUMJGNGAK       PIC  9(06).
      *    ������������ݷ�ݾ�²��
                  09 HKNWKV01-TSY-NYUMJGNCNT       PIC  9(02).
      *    ������������ݿ������ܶ�ʬ
                  09 HKNWKV01-TSY-NYUSKJRYYKBN     PIC  X(01).
      *��ǯ��
              05 HKNWKV01-TNK.
      *  ��ǯ��ݳ������ô��ʬ
                07 HKNWKV01-TNK-GAIFTNKBN         PIC  X(01).
      *  ��ǯ��ݳ���������
                07 HKNWKV01-TNK-GAI-NAIYO.
      *    ��ǯ��ݳ���ݲ����ô���
                  09 HKNWKV01-TNK-GAIKFTNRATE      PIC  9(03).
      *    ��ǯ��ݳ���ݲ�ݸ����
                  09 HKNWKV01-TNK-GAIKKOTGAK       PIC  9(06).
      *    ��ǯ��ݳ���ݲ�ݾ�³�
                  09 HKNWKV01-TNK-GAIKJGNGAK       PIC  9(06).
      *    ��ǯ��ݳ�������ݾ�³�
                  09 HKNWKV01-TNK-GAIDJGNGAK       PIC  9(06).
      *    ��ǯ��ݳ�������ݾ�²��
                  09 HKNWKV01-TNK-GAIDJGNCNT       PIC  9(02).
      *    ��ǯ��ݳ���ݷ�ݱ���ݾ�³�
                  09 HKNWKV01-TNK-GAIMNAIJGNGAK    PIC  9(06).
      *    ��ǯ��ݳ���ݷ�ݱ����ݾ�³�
                  09 HKNWKV01-TNK-GAIMGAIJGNGAK    PIC  9(06).
      *    ��ǯ��ݳ���ݷ�ݾ�²��
                  09 HKNWKV01-TNK-GAIMJGNCNT       PIC  9(02).
      *    ��ǯ��ݳ����������ô��ʬ
                  09 HKNWKV01-TNK-GAIYKZFTNKBN     PIC  X(01).
      *  ��ǯ�����������ô��ʬ
                07 HKNWKV01-TNK-NYUFTNKBN        PIC  X(01).
      *  ��ǯ�������������
                07 HKNWKV01-TNK-NYU-NAIYO.
      *    ��ǯ��������ݲ����ô���
                  09 HKNWKV01-TNK-NYUKFTNRATE      PIC  9(03).
      *    ��ǯ��������ݲ�ݸ����
                  09 HKNWKV01-TNK-NYUKKOTGAK       PIC  9(06).
      *    ��ǯ��������ݲ�ݾ�³�
                  09 HKNWKV01-TNK-NYUKJGNGAK       PIC  9(06).
      *    ��ǯ������������ݾ�³�
                  09 HKNWKV01-TNK-NYUDJGNGAK       PIC  9(06).
      *    ��ǯ������������ݾ�²��
                  09 HKNWKV01-TNK-NYUDJGNCNT       PIC  9(02).
      *    ��ǯ��������ݷ�ݾ�³�
                  09 HKNWKV01-TNK-NYUMJGNGAK       PIC  9(06).
      *    ��ǯ��������ݷ�ݾ�²��
                  09 HKNWKV01-TNK-NYUMJGNCNT       PIC  9(02).
      *    ��ǯ��������ݿ������ܶ�ʬ
                  09 HKNWKV01-TNK-NYUSKJRYYKBN     PIC  X(01).
      *
           03 HKNWKV01-NAIYOU-R        REDEFINES   HKNWKV01-NAIYOU.
              05 HKNWKV01-TBL                  OCCURS  4.
      *  �������ô��ʬ
                07 HKNWKV01-GAIFTNKBN          PIC  X(01).
      *  ���������
                07 HKNWKV01-GAI-NAIYO.
      *    ����ݲ����ô���
                  09 HKNWKV01-GAIKFTNRATE         PIC  9(03).
      *    ����ݲ�ݸ����
                  09 HKNWKV01-GAIKKOTGAK          PIC  9(06).
      *    ����ݲ�ݾ�³�
                  09 HKNWKV01-GAIKJGNGAK          PIC  9(06).
      *    ��������ݾ�³�
                  09 HKNWKV01-GAIDJGNGAK          PIC  9(06).
      *    ��������ݾ�²��
                  09 HKNWKV01-GAIDJGNCNT          PIC  9(02).
      *    ����ݷ�ݱ���ݾ�³�
                  09 HKNWKV01-GAIMNAIJGNGAK       PIC  9(06).
      *    ����ݷ�ݱ����ݾ�³�
                  09 HKNWKV01-GAIMGAIJGNGAK       PIC  9(06).
      *    ����ݷ�ݾ�²��
                  09 HKNWKV01-GAIMJGNCNT          PIC  9(02).
      *    �����������ô��ʬ
                  09 HKNWKV01-GAIYKZFTNKBN        PIC  X(01).
      *  ��������ô��ʬ
                07 HKNWKV01-NYUFTNKBN             PIC  X(01).
      *  ����������
                07 HKNWKV01-NYU-NAIYO.
      *    �����ݲ����ô���
                  09 HKNWKV01-NYUKFTNRATE         PIC  9(03).
      *    �����ݲ�ݸ����
                  09 HKNWKV01-NYUKKOTGAK          PIC  9(06).
      *    �����ݲ�ݾ�³�
                  09 HKNWKV01-NYUKJGNGAK          PIC  9(06).
      *    ���������ݾ�³�
                  09 HKNWKV01-NYUDJGNGAK          PIC  9(06).
      *    ���������ݾ�²��
                  09 HKNWKV01-NYUDJGNCNT          PIC  9(02).
      *    �����ݷ�ݾ�³�
                  09 HKNWKV01-NYUMJGNGAK          PIC  9(06).
      *    �����ݷ�ݾ�²��
                  09 HKNWKV01-NYUMJGNCNT          PIC  9(02).
      *    �����ݿ������ܶ�ʬ
                  09 HKNWKV01-NYUSKJRYYKBN        PIC  X(01).
      *����ü���ɣ�
           03 HKNWKV01-TERMID                  PIC  X(16).
      *�������ڡ��졼���ɣ�
           03 HKNWKV01-OPID                    PIC  X(16).
      *��������ǯ����
           03 HKNWKV01-CREYMD                  PIC  X(08).
      *��������ǯ����
           03 HKNWKV01-UPYMD                   PIC  X(08).
      *������������
           03 HKNWKV01-UPHMS                   PIC  X(06).
