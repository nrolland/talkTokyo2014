public interface IEnumerator
{
    bool MoveNext();
    Object Current {
        get; 
    }
    void Reset();
}
public interface IEnumerable
{
    IEnumerator GetEnumerator();
}