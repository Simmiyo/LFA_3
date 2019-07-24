#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <queue>
#include <algorithm>
#include <fstream>
#include <regex>

using namespace std;

typedef pair < string, vector<string> > productie;

vector <string> fragmentare(string lista, const string delim)
{
    vector <string> fragmente;
    int poz = 0;
    string token;
    while ((poz = lista.find(delim)) !=  string::npos)
    {
        token = lista.substr(0,poz);
        fragmente.push_back(token);
        lista.erase(0,poz + delim.length());
    }
    lista.erase(lista.length());
    fragmente.push_back(lista);
    return fragmente;
}

void sterge_net(string & sir, const string & net)
{
    int poz = string::npos;
	while ((poz  = sir.find(net))!= string::npos)
	{
        sir.erase(poz,net.length());
	}
}

vector<string> rremove(string &sir,const string &subsir)
{
    int i = sir.find(subsir), k, j, n;
    vector<int> poz;
    set <string> auxiliar;
    string aux;
    while(i != string::npos)
    {
        poz.push_back(i);
        i = sir.find(subsir, subsir.length() + i);
    }
    n = poz.size();
    if(n)
    {
        vector<bool> comb(n, 0);
        for(k=1;k<=n;k++)
        {
            fill(comb.begin(), comb.begin() + k, 1);
            do
            {
                aux = sir;
                for(j=n-1;j>=0;j--)
                {
                    if(comb[j])
                    {
                        aux.erase(aux.begin()+poz[j]);
                    }
                }
                auxiliar.insert(aux);
            }while(prev_permutation(comb.begin(),comb.end()));
            fill(comb.begin(),comb.end(),0);
        }
    }
    vector<string> rez(auxiliar.begin(),auxiliar.end());
    return rez;
}

bool verif_neterminal(string &l)
{
    return (l.length() == 1 && l >= "A" && l <= "Z");
}

bool verif_terminal(string &l)
{
    return (l.length() == 1 && l >= "a" && l <= "z");
}

unsigned lungime(string &s)
{
    unsigned nr = 0;
    for(auto x: s)
        if(x >= 'A' && x <= 'Z')
            nr++;
    return nr;
}

string scoate_ultimul_net(string &s)
{
    string s1;
    unsigned i=0;
    for(i=s.length();i>0;i--)
        if(s[i]>='A' && s[i]<='Z')
            break;
    s1 = s.substr(0,i);
    s.erase(s.begin(),s.begin()+i);
    return s1;
}

bool only_terminals(string &s)
{
    for(auto x:s)
        if(x<='a'||x>='z')
            return 0;
    return 1;
}

vector<string> scoate_neterminale(string deriv)
{
    vector<string> neterminale;
    string s;
    for(int i=0;i<deriv.length();i++)
    {
        s += deriv[i];
        if(verif_neterminal(s))
            neterminale.push_back(s);
        s.clear();
    }
    return neterminale;
}

void inlocuieste(string & sir, string subsir_vechi, string subsir_nou)
{
	string sir_nou = "";
	for(auto x: sir)
	{
        if(x == subsir_vechi[0])
            sir_nou += subsir_nou;
        else sir_nou += x;
	}
	sir = sir_nou;
}

vector<string> scoate_terminale(string deriv)
{
    vector <string> terminale;
    string s;
    for(int i=0;i<deriv.length();i++)
    {
        s += deriv[i];
        if(verif_terminal(s))
            terminale.push_back(s);
        s.clear();
    }
    return terminale;
}

class GIC
{
    map< string, vector<string> > P;
    string start;
    public:
        friend istream & operator>>(istream &,GIC &);
        friend ostream & operator<<(ostream &,const GIC &);
        void new_start();
        void elim_prod_vide();
        void elim_redenum();
        void elim_prod_inutil();
        vector<string> gaseste_pord_intermin();
        void elim_variabila(vector <string> &);
        vector<string> gasete_prod_inaccesibile();
        void new_prod(string, const string, int);
        void separa_terminale();
        void separa_neterminale();
};

void GIC :: new_prod(string camp1,const string camp2, int it = -1)
{
    productie p;
    p.second.push_back(camp1);
    string neterminal = camp2;
    if(it>=0)
        neterminal += to_string(it);
    p.first = neterminal;
    P.insert(p);
}

void GIC :: new_start()
{
    bool k = 0;
    for(auto x: P)
    {
        for(auto y: x.second)
            if(y.find(start) != string::npos)
            {
                string nou = start + "o";
                new_prod(start,nou);
                start = nou;
                k = 1;
                break;
            }
        if(k)
            break;
    }
}

void GIC :: elim_prod_vide()
{
    bool k;
    map< string, vector<string> > :: iterator it_map,it_map1;
    int i = 0, j = 0, poz, p = 0;
    do{
        k = 0;
        it_map = P.begin();
        while(it_map != P.end())
        {
            j = 0;
            while(j < it_map->second.size())
            {
                if(it_map->second[j] == "@")
                {
                    for(it_map1=P.begin();it_map1!=P.end();++it_map1)
                    {
                        p = 0;
                        for(i=0;i<it_map1->second.size()-p;i++)
                            if((poz = it_map1->second[i].find(it_map->first)) != string::npos)
                            {
                                if(it_map->second.size() == 1)
                                {
                                    it_map1->second[i].erase(it_map1->second[i].begin()+poz);
                                    if(!(it_map1->second[i]).length())
                                        it_map1->second[i] = "@";
                                }
                                else
                                {
                                    string copie = it_map1->second[i];
                                    vector <string> prod;
                                    prod = rremove(copie,it_map->first);
                                    if(!prod[0].length())
                                        prod[0]="@";
                                    it_map1->second.insert(it_map1->second.end(), prod.begin(), prod.end());
                                    p += prod.size();
                                }
                            }
                    }
                    if(it_map->second.size() == 1)
                    {
                        P.erase(it_map);
                        it_map--;
                        j = 0;
                    }
                    else
                        {
                            it_map->second.erase(it_map->second.begin()+j);
                            j--;
                        }
                    k = 1;
                }
                j++;
            }
            it_map++;
        }
    }while(k);
}

void GIC :: elim_redenum()
{
    int i, n;
    map< string, vector<string> > :: iterator it_map;
    queue<string> coada;
    set<string> auxiliar;
    string neterminal;
    for(auto &x: P)
    {
        i = 0;
        while(i < x.second.size())
        {
            if(verif_neterminal(x.second[i]))
            {
                coada.push(x.second[i]);
                x.second.erase(x.second.begin()+i);
            }
            else
                i++;
        }
        if(!coada.empty())
        {
            copy(x.second.begin(),x.second.end(),inserter(auxiliar,auxiliar.end()));
            x.second.clear();
            do
            {
                neterminal = coada.front();
                coada.pop();
                if(neterminal == x.first) continue;
                it_map = P.find(neterminal);
                for(auto y : it_map->second)
                {
                    if(verif_neterminal(y))
                        coada.push(y);
                    else
                        auxiliar.insert(y);
                }
            }while(!coada.empty());
            copy(auxiliar.begin(), auxiliar.end(), back_inserter(x.second));
            auxiliar.clear();
        }
    }
}

vector<string> GIC :: gaseste_pord_intermin()
{
    map< string, vector <string> > interm;
    vector<string> termin,neterminale,net;
    map< string, vector <string> > :: iterator it_map;
    bool ok = 0;
    int n;
    for(auto x: P)
    {
        ok = 0;
        for(auto y: x.second)
        {
            if(verif_terminal(y) || only_terminals(y))
            {
                termin.push_back(x.first);
                ok = 1;
                break;
            }
        }
        if(!ok)
            interm.insert(x);
    }
    do
    {
         n = termin.size();
         it_map = interm.begin();
         while(it_map != interm.end())
         {
            for(auto y: it_map->second)
            {
                neterminale = scoate_neterminale(y);
                if(includes(termin.begin(),termin.end(),neterminale.begin(),neterminale.end()))
                {
                    termin.push_back(it_map->first);
                    interm.erase(it_map);
                    it_map--;
                    break;
                }
            }
            it_map++;
        }
    }while(n!=termin.size());
    for(auto x: interm)
        net.push_back(x.first);
    return net;
}

vector <string> GIC :: gasete_prod_inaccesibile()
{
    map< string, vector<string> > :: iterator it_map = P.begin();
    vector<string> temp;
    string net;
    set<string> net_gasite;
    net_gasite.insert(start);
    queue<string> net_accesate;
    net_accesate.push(start);
    int n;
    while(!net_accesate.empty())
    {
        net = net_accesate.front();
        net_accesate.pop();
        it_map = P.find(net);
        for(auto x: it_map->second)
        {
            if(!verif_terminal(x) && !only_terminals(x))
            {
                if(!verif_neterminal(x))
                {
                    temp = scoate_neterminale(x);
                    n = net_gasite.size();
                    for(auto y: temp)
                    {
                        net_gasite.insert(y);
                        if(net_gasite.size() != n)
                        {
                            net_accesate.push(y);
                            n++;
                        }
                    }
                }
                else if(net_gasite.find(x) == net_gasite.end())
                        {
                            net_gasite.insert(x);
                            net_accesate.push(x);
                        }
            }
        }
    }
    temp.clear();
    for(auto x: P)
    {
        if(net_gasite.find(x.first) == net_gasite.end())
            temp.push_back(x.first);
    }
    return temp;
}

void GIC ::elim_variabila(vector <string> &net)
{
    map< string, vector<string> > :: iterator it_map = P.begin();
    for(auto x: net)
    {
        while(it_map != P.end())
        {
            if(it_map->first == x)
            {
                P.erase(it_map);
                break;
            }
            it_map++;
        }
        it_map = P.begin();
    }
    for(auto y: net)
    {
        int i;
        for(auto &x: P)
        {
            for(i=0;i<x.second.size();i++)
            {
                sterge_net(x.second[i],y);
                if(x.second[i].empty())
                    x.second.erase(x.second.begin()+i);
            }
        }
    }
}

void GIC :: elim_prod_inutil()
{
    vector <string> prod_inut;
    prod_inut = gaseste_pord_intermin();
    elim_variabila(prod_inut);
    prod_inut = gasete_prod_inaccesibile();
    map< string, vector<string> > :: iterator it_map = P.begin();
    for(auto x: prod_inut)
    {
        while(it_map != P.end())
        {
            if(it_map->first == x)
            {
                P.erase(it_map);
                break;
            }
            it_map++;
        }
        it_map = P.begin();
    }
}

void GIC :: separa_terminale()
{
    vector<string> ter;
    int i, k = 1;
    GIC temp;
    bool ok;
    for(auto &x: P)
    {
        for(i=0;i<x.second.size();i++)
        {
            if(!verif_terminal(x.second[i]))
            {
                ter = scoate_terminale(x.second[i]);
                for(auto y: ter)
                {
                    ok = 0;
                    for(auto z: temp.P)
                        if(y == z.second[0])
                        {
                            inlocuieste(x.second[i], y, z.first);
                            ok = 1;
                            break;
                        }
                    if(!ok)
                    {
                        temp.new_prod(y,"Y",k);
                        inlocuieste(x.second[i], y, "Y" + to_string(k));
                        k++;
                    }
                }
            }
        }
    }
    for(auto x: temp.P)
        P.insert(x);
}

void GIC :: separa_neterminale()
{
    map< string, vector<string> > :: iterator it_map;
    int i = 0, n;
    bool ok = 0;
    GIC temp1,temp2;
    do{
        n = temp2.P.size();
        it_map = P.begin();
        while(it_map != P.end())
        {
            for(auto &x: it_map->second)
                if(!verif_terminal(x) && lungime(x) > 2)
                {
                    string sir = scoate_ultimul_net(x);
                    ok = 0;
                    for(auto y: temp1.P)
                        if(y.second[0] == sir)
                        {
                            x = y.first + x;
                            ok = 1;
                            break;
                        }
                    if(!ok)
                    {
                        new_prod(sir,"X",i);
                        temp1.new_prod(sir,"X",i);
                        x = "X" + to_string(i) + x;
                        i++;
                    }
                }
            temp2.P.insert(*it_map);
            it_map = P.erase(it_map);
        }
    }while(n != temp2.P.size());
    string s = start;
    *this = temp2;
    for(auto x: temp1.P)
        P.insert(x);
    start = s;
}

istream & operator>>(istream &in, GIC &p)
{
    in >> p.start;
    string linie;
    productie pr;
    vector<string> aux(2);
    in.ignore(1,'\n');
    while(getline(in,linie))
    {
        aux = fragmentare(linie," -> ");
        pr.first = aux[0];
        pr.second = fragmentare(aux[1]," | ");
        p.P.insert(pr);
    }
    return in;
}

ostream & operator<<(ostream &out,const GIC &p)
{
    out << "Reguli de productie:" << endl;
    for(auto x: p.P)
    {
        out << x.first << " -> ";
        for(auto y: x.second)
        {
             out << y;
             if(y != x.second.back())
                out << " | ";
        }
        out << endl;
    }
    out << "Simbol de start: " << p.start << endl;
    return out;
}

int main()
{
    ifstream f;
    f.open("gramatica");
    GIC g;
    f >> g;
    g.new_start();
    g.elim_prod_vide();
    g.elim_redenum();
    g.elim_prod_inutil();
    g.separa_terminale();
    g.separa_neterminale();
    cout << g;
    f.close();
    return 0;
}
