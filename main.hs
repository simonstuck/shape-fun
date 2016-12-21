type Point = (Double, Double)
type Vertex = Point
type Line = (Point, Point)
type Polygon = [Vertex]
type Radius = Double
type Circle = (Point, Radius)

-- test data
polygon = [(1,1), (1, 2), (2, 2), (2, 1)]

pointA = (1.5, 1.5)
pointB = (3.0, 3.0)
pointC = (1.0, 1.0)
pointD = (1.0, 1.5)
pointE = (0.9, 0.9)

lineA = ((1.0,1.0), (2.0,2.0))

circleA = (piontA, 3.0)

-- some fun methods
isOnLine :: Line -> Point -> Bool
isOnLine ((xi, yi), (xj, yj)) (x, y) = y' == y && within
  where
    y' = m * x + b
    m = (yj - yi) / (xj - xi)
    b = yi - (m * xi)
    within = (x < xi) /= (x < xj) && (y < yi) /= (y < yj)

-- https://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
isInPolygon :: Polygon -> Point -> Bool
isInPolygon poly p = odd $ sum $ map fromEnum $ crossesPath poly p
  where
    crossesPath [(_, _)] _ = []
    crossesPath ((xi, yi) : vs@((xj, yj) : vs')) p@(x, y) = inter : crossesPath vs p
      where
        inter = ((yi > y) /= (yj > y)) && (x < (xj - xi) * (y - yi) / (yj - yi) + xi)


isInCircle :: Circle -> Point -> Bool
-- check if distance between points is <= r
isInCircle ((xi, yi), r) (x, y) = d <= r
  where 
    d = sqrt $ (x - xi)**2 + (y - yi) ** 2
  
